use poem_openapi::{payload::PlainText, OpenApi};

pub struct HealthCheck;

#[OpenApi]
impl HealthCheck {
    #[oai(path = "/health_check", method = "get")]
    async fn health_check(&self) -> HealthCheckResponse {
        HealthCheckResponse::Ok(PlainText("moist".to_string()))
    }
}

#[derive(poem_openapi::ApiResponse)]
pub enum HealthCheckResponse {
    #[oai(status = 200)]
    Ok(PlainText<String>),
}
