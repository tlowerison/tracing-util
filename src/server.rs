use crate::*;
use ::axum::{async_trait, extract::FromRequestParts};
use ::axum_client_ip::{SecureClientIp, SecureClientIpSource};
use ::derivative::Derivative;
use ::derive_more::*;
use ::http::{request::Parts, HeaderName, Request, StatusCode};
use ::tower_http::request_id::MakeRequestId;
use ::uuid::Uuid;

pub static X_REQUEST_ID: HeaderName = HeaderName::from_static("x-request-id");

/// NOTE: this struct cannot be extracted with an Extension, it can only be extracted with a TypedHeader
/// suggested usage: if using an Axum ServiceBuilder, add a call
/// ```rust
/// .set_request_id(service_util::X_REQUEST_ID, service_util::RequestId::default())
/// ```
#[derive(
    Clone,
    Copy,
    Default,
    Deref,
    Derivative,
    Deserialize,
    Eq,
    From,
    Into,
    Ord,
    PartialEq,
    PartialOrd,
    Serialize,
)]
#[derivative(Debug = "transparent")]
pub struct RequestId(pub Uuid);

impl MakeRequestId for RequestId {
    fn make_request_id<B>(
        &mut self,
        _: &Request<B>,
    ) -> Option<::tower_http::request_id::RequestId> {
        let request_id = Uuid::new_v4().to_string().parse().unwrap();
        Some(::tower_http::request_id::RequestId::new(request_id))
    }
}

fn from_request_parts_error() -> (StatusCode, &'static str) {
    (
        StatusCode::BAD_REQUEST,
        "`x-request-id` header is malformed, expected a uuid",
    )
}

#[async_trait]
impl<S> FromRequestParts<S> for RequestId
where
    S: Send + Sync,
{
    type Rejection = (StatusCode, &'static str);

    async fn from_request_parts(parts: &mut Parts, _: &S) -> Result<Self, Self::Rejection> {
        if let Some(x_request_id) = parts.headers.get(&X_REQUEST_ID) {
            let x_request_id = x_request_id
                .to_str()
                .map_err(|_| from_request_parts_error())?;

            match Uuid::parse_str(x_request_id) {
                Ok(request_id) => Ok(Self(request_id)),
                Err(_) => Err(from_request_parts_error()),
            }
        } else {
            Err((StatusCode::BAD_REQUEST, "`x-request-id` header is missing"))
        }
    }
}

pub mod make_span {
    use super::*;

    pub fn debug<B>(
        secure_client_ip_source: SecureClientIpSource,
    ) -> impl Clone + Fn(&Request<B>) -> Span {
        move |req| {
            set_trace_parent(
                req.headers(),
                tracing::debug_span!(
                    target: "",
                    "request",
                    "http.method" = %req.method(),
                    "http.target" = %req.uri(),
                    "http.client_ip" = get_client_ip(&secure_client_ip_source, req).map(|SecureClientIp(client_ip)| client_ip.to_string()),
                ),
            )
        }
    }

    pub fn error<B>(
        secure_client_ip_source: SecureClientIpSource,
    ) -> impl Clone + Fn(&Request<B>) -> Span {
        move |req| {
            set_trace_parent(
                req.headers(),
                tracing::error_span!(
                    target: "",
                    "request",
                    "http.method" = %req.method(),
                    "http.target" = %req.uri(),
                    "http.client_ip" = get_client_ip(&secure_client_ip_source, req).map(|SecureClientIp(client_ip)| client_ip.to_string()),
                ),
            )
        }
    }

    pub fn info<B>(
        secure_client_ip_source: SecureClientIpSource,
    ) -> impl Clone + Fn(&Request<B>) -> Span {
        move |req| {
            set_trace_parent(
                req.headers(),
                tracing::info_span!(
                    target: "",
                    "request",
                    "http.method" = %req.method(),
                    "http.target" = %req.uri(),
                    "http.client_ip" = get_client_ip(&secure_client_ip_source, req).map(|SecureClientIp(client_ip)| client_ip.to_string()),
                ),
            )
        }
    }

    pub fn trace<B>(
        secure_client_ip_source: SecureClientIpSource,
    ) -> impl Clone + Fn(&Request<B>) -> Span {
        move |req| {
            set_trace_parent(
                req.headers(),
                tracing::trace_span!(
                    target: "",
                    "request",
                    "http.method" = %req.method(),
                    "http.target" = %req.uri(),
                    "http.client_ip" = get_client_ip(&secure_client_ip_source, req).map(|SecureClientIp(client_ip)| client_ip.to_string()),
                ),
            )
        }
    }

    pub fn warn<B>(
        secure_client_ip_source: SecureClientIpSource,
    ) -> impl Clone + Fn(&Request<B>) -> Span {
        move |req| {
            set_trace_parent(
                req.headers(),
                tracing::warn_span!(
                    target: "",
                    "request",
                    "http.method" = %req.method(),
                    "http.target" = %req.uri(),
                    "http.client_ip" = get_client_ip(&secure_client_ip_source, req).map(|SecureClientIp(client_ip)| client_ip.to_string()),
                ),
            )
        }
    }

    fn get_client_ip<B>(
        secure_client_ip_source: &SecureClientIpSource,
        req: &Request<B>,
    ) -> Option<SecureClientIp> {
        SecureClientIp::from(secure_client_ip_source, req.headers(), req.extensions()).ok()
    }
}
