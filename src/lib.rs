#[cfg(feature = "server")]
mod server;

#[cfg(feature = "server")]
pub use server::*;

use ::chrono::Utc;
use ::http::header::{HeaderMap, HeaderName};
use ::opentelemetry::propagation::TextMapPropagator;
use ::opentelemetry::trace::TraceContextExt;
use ::opentelemetry_sdk::propagation::TraceContextPropagator;
use ::serde::*;
use ::std::collections::HashMap;
use ::std::str::FromStr;
use ::tracing::Span;
use ::tracing_error::ErrorLayer;
use ::tracing_log::LogTracer;
use ::tracing_opentelemetry::{OpenTelemetryLayer, OpenTelemetrySpanExt};
use ::tracing_subscriber::filter::{targets::Targets, LevelFilter};
use ::tracing_subscriber::{layer::SubscriberExt, EnvFilter, Registry};
use ::tracing_tree::time::FormatTime;

#[derive(Clone, Copy, Debug)]
pub struct UTCTime;

#[derive(Clone, Copy, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum JaegerSinkKind {
    Agent,
    #[default]
    Collector,
}

#[macro_export]
macro_rules! instrument_field {
    ($ident:ident) => {
        tracing::Span::current().record(stringify!($ident), &&*format!("{:?}", $ident));
    };
    ($name:literal, $expr:expr) => {
        tracing::Span::current().record($name, &&*format!("{:?}", $expr));
    };
}

// macro because typically $registry is a deeply nested type
// which would have an unknown amount of required type parameters
// if being accepted as a function argument
macro_rules! set_global_default {
    ($registry:expr) => {
        tracing::subscriber::set_global_default($registry).expect("failed to set subscriber")
    };
}

pub static TRACEPARENT: HeaderName = HeaderName::from_static("traceparent");
pub static TRACESTATE: HeaderName = HeaderName::from_static("tracestate");

#[derive(Deserialize)]
struct Config {
    jaeger_sink_kind: Option<JaegerSinkKind>,
    log_hierarchical_layer_ansi: Option<bool>,
    log_hierarchical_layer_bracketed_fields: Option<bool>,
    log_hierarchical_layer_indent_amount: Option<usize>,
    log_hierarchical_layer_indent_lines: Option<bool>,
    log_hierarchical_layer_targets: Option<bool>,
    log_hierarchical_layer_thread_ids: Option<bool>,
    log_hierarchical_layer_thread_names: Option<bool>,
    log_hierarchical_layer_verbose_entry: Option<bool>,
    log_hierarchical_layer_verbose_exit: Option<bool>,
    log_hierarchical_layer_wraparound: Option<usize>,
    #[serde(deserialize_with = "deserialize_log_target_default_level")]
    log_target_default_level: Option<LevelFilter>,
    #[serde(deserialize_with = "deserialize_log_target_filters")]
    log_target_filters: Option<Targets>,
    otel_enabled: Option<bool>,
    otel_event_attribute_count_limit: Option<u32>,
    otel_link_attribute_count_limit: Option<u32>,
    // OpenTelemetry environment variables which are already handled by
    // the opentelemetry and opentelemetry_jaeger libraries:
    // - OTEL_EXPORTER_JAEGER_ENDPOINT: defaults to "http://localhost:14250/api/trace"
    // - OTEL_EXPORTER_JAEGER_USER
    // - OTEL_EXPORTER_JAEGER_PASSWORD
    // - OTEL_EXPORTER_JAEGER_TIMEOUT: defaults to 10 seconds
    // - OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT: defaults to 128
    // - OTEL_SPAN_EVENT_COUNT_LIMIT: defaults to 128
    // - OTEL_SPAN_LINK_COUNT_LIMIT: defaults to 128
    // - OTEL_TRACES_SAMPLER: defaults to "parentbased_always_on"
    // - OTEL_TRACES_SAMPLER_ARG:
    // - OTEL_BSP_SCHEDULE_DELAY: defaults to 5 seconds
    // - OTEL_BSP_MAX_QUEUE_SIZE: defaults to 2048
    // - OTEL_BSP_MAX_EXPORT_BATCH_SIZE: defaults to 512
    // - OTEL_BSP_EXPORT_TIMEOUT: defaults to 30 seconds
    // - OTEL_BSP_MAX_CONCURRENT_EXPORTS: defaults to 1
}

pub fn install_tracing() {
    let config = envy::from_env::<Config>().expect("unable to parse environment variables");

    let jaeger_sink_kind = config.jaeger_sink_kind.unwrap_or_default();

    let log_hierarchical_layer_ansi = config.log_hierarchical_layer_ansi;

    let log_hierarchical_layer_bracketed_fields = config
        .log_hierarchical_layer_bracketed_fields
        .unwrap_or(true);

    let log_hierarchical_layer_indent_amount =
        config.log_hierarchical_layer_indent_amount.unwrap_or(2);

    let log_hierarchical_layer_indent_lines =
        config.log_hierarchical_layer_indent_lines.unwrap_or(true);

    let log_hierarchical_layer_targets = config.log_hierarchical_layer_targets.unwrap_or(true);

    let log_hierarchical_layer_thread_ids =
        config.log_hierarchical_layer_thread_ids.unwrap_or(false);

    let log_hierarchical_layer_thread_names =
        config.log_hierarchical_layer_thread_names.unwrap_or(false);

    let log_hierarchical_layer_verbose_entry =
        config.log_hierarchical_layer_verbose_entry.unwrap_or(false);

    let log_hierarchical_layer_verbose_exit =
        config.log_hierarchical_layer_verbose_exit.unwrap_or(false);

    let log_hierarchical_layer_wraparound = config.log_hierarchical_layer_wraparound;

    let log_target_default_level = config.log_target_default_level.unwrap_or(LevelFilter::OFF);

    let log_target_filters = config.log_target_filters;

    let otel_enabled = config.otel_enabled.unwrap_or(false);

    LogTracer::init().expect("unable to initialize LogTracer");

    let global_targets_filter =
        log_target_filters.map(|x| x.with_default(log_target_default_level));

    let registry = Registry::default()
        .with(EnvFilter::from_default_env())
        .with({
            let mut layer =
                ::tracing_tree::HierarchicalLayer::new(log_hierarchical_layer_indent_amount)
                    .with_bracketed_fields(log_hierarchical_layer_bracketed_fields)
                    .with_indent_lines(log_hierarchical_layer_indent_lines)
                    .with_targets(log_hierarchical_layer_targets)
                    .with_thread_ids(log_hierarchical_layer_thread_ids)
                    .with_thread_names(log_hierarchical_layer_thread_names)
                    .with_verbose_entry(log_hierarchical_layer_verbose_entry)
                    .with_verbose_exit(log_hierarchical_layer_verbose_exit)
                    .with_timer(UTCTime);

            if let Some(ansi) = log_hierarchical_layer_ansi {
                layer = layer.with_ansi(ansi);
            }
            if let Some(wraparound) = log_hierarchical_layer_wraparound {
                layer = layer.with_wraparound(wraparound);
            }
            layer
        });

    match otel_enabled {
        false => match global_targets_filter {
            None => set_global_default!(registry.with(ErrorLayer::default())),
            Some(filter) => set_global_default!(registry.with(filter).with(ErrorLayer::default())),
        },
        true => match global_targets_filter {
            None => set_global_default!(registry
                .with(OpenTelemetryLayer::new(jaeger_tracer(
                    jaeger_sink_kind,
                    config.otel_event_attribute_count_limit,
                    config.otel_link_attribute_count_limit
                )))
                .with(ErrorLayer::default())),
            Some(filter) => set_global_default!(registry
                .with(filter)
                .with(OpenTelemetryLayer::new(jaeger_tracer(
                    jaeger_sink_kind,
                    config.otel_event_attribute_count_limit,
                    config.otel_link_attribute_count_limit
                )))
                .with(ErrorLayer::default())),
        },
    };

    opentelemetry::global::set_text_map_propagator(TraceContextPropagator::new());
}

fn jaeger_tracer(
    jaeger_sink_kind: JaegerSinkKind,
    otel_event_attribute_count_limit: Option<u32>,
    otel_link_attribute_count_limit: Option<u32>,
) -> opentelemetry_sdk::trace::Tracer {
    let mut config = opentelemetry_sdk::trace::Config::default();

    if let Some(max_attributes) = otel_event_attribute_count_limit {
        config = config.with_max_attributes_per_event(max_attributes);
    }
    if let Some(max_attributes) = otel_link_attribute_count_limit {
        config = config.with_max_attributes_per_link(max_attributes);
    }

    match jaeger_sink_kind {
        JaegerSinkKind::Agent => opentelemetry_jaeger::new_agent_pipeline()
            .with_trace_config(config)
            .install_batch(opentelemetry_sdk::runtime::Tokio)
            .expect("unable to install Jaeger Agent pipeline"),
        JaegerSinkKind::Collector => opentelemetry_jaeger::new_collector_pipeline()
            .with_trace_config(config)
            .with_hyper()
            .install_batch(opentelemetry_sdk::runtime::Tokio)
            .expect("unable to install Jaeger Collector pipeline"),
    }
}

pub fn set_trace_parent(headers: &HeaderMap, span: Span) -> Span {
    let propagator = TraceContextPropagator::new();
    if let Some(traceparent) = headers.get(&TRACEPARENT).and_then(|x| x.to_str().ok()) {
        // Propagator::extract only works with HashMap<String, String>
        let mut propagated_headers = match headers.get(&TRACESTATE).and_then(|x| x.to_str().ok()) {
            Some(tracestate) => {
                let mut propagated_headers = HashMap::with_capacity(2);
                propagated_headers.insert("tracestate".to_string(), tracestate.to_string());
                propagated_headers
            }
            None => HashMap::with_capacity(1),
        };
        propagated_headers.insert("traceparent".to_string(), traceparent.to_string());

        let context = propagator.extract(&propagated_headers);
        span.set_parent(context);
    }
    span
}

pub fn traceparent() -> Option<String> {
    let context = Span::current().context();
    let span_ref = context.span();
    let span_context = span_ref.span_context();
    if !span_context.is_valid() {
        return None;
    }
    let trace_id = span_context.trace_id();
    let span_id = span_context.span_id();
    let flags = span_context.trace_flags().to_u8();
    Some(format!("00-{trace_id}-{span_id}-{flags:02x}"))
}

impl FormatTime for UTCTime {
    fn format_time(&self, w: &mut impl std::fmt::Write) -> std::fmt::Result {
        write!(w, "{}", Utc::now().format("%+"))
    }
}

impl std::str::FromStr for JaegerSinkKind {
    type Err = ::anyhow::Error;
    fn from_str(str: &str) -> Result<Self, ::anyhow::Error> {
        Ok(match str {
            "agent" => Self::Agent,
            "collector" => Self::Collector,
            _ => {
                return Err(::anyhow::Error::msg(format!(
                    "unrecognized JaegerSinkKind variant: {str}"
                )))
            }
        })
    }
}

fn deserialize_log_target_default_level<'de, D>(de: D) -> Result<Option<LevelFilter>, D::Error>
where
    D: ::serde::de::Deserializer<'de>,
{
    Option::<&str>::deserialize(de)?
        .map(FromStr::from_str)
        .transpose()
        .map_err(::serde::de::Error::custom)
}

fn deserialize_log_target_filters<'de, D>(de: D) -> Result<Option<Targets>, D::Error>
where
    D: ::serde::de::Deserializer<'de>,
{
    Option::<&str>::deserialize(de)?
        .map(FromStr::from_str)
        .transpose()
        .map_err(::serde::de::Error::custom)
}
