use poem::{
    http::StatusCode,
    session::CookieSession,
    test::{TestClient, TestResponse},
    Endpoint, EndpointExt, Route,
};
use poem_openapi::OpenApiService;
use uuid::Uuid;

use crate::db;

pub async fn setup() -> TestClient<impl Endpoint> {
    let db = db::connect().await;
    static DB_SETUP_ONCE: tokio::sync::OnceCell<()> = tokio::sync::OnceCell::const_new();
    DB_SETUP_ONCE
        .get_or_init(|| async {
            db.collection::<()>(super::Users::TABLE)
                .drop(None)
                .await
                .unwrap();
        })
        .await;

    let users = OpenApiService::new(super::Users::new(db).await, "users", "1.0.0");
    TestClient::new(
        Route::new()
            .nest("/", users)
            .with(CookieSession::new(super::cookie_config())),
    )
}

async fn register(client: &TestClient<impl Endpoint>, name: &str) -> TestResponse {
    client
        .post("/register")
        .content_type("application/x-www-form-urlencoded")
        .body(format!("name={}", name))
        .send()
        .await
}

#[tokio::test]
async fn test_register() {
    let client = setup().await;

    let user = "alec";

    let too_long = "a".repeat(super::MAX_NAME_LEN + 1);
    register(&client, too_long.as_str())
        .await
        .assert_status(StatusCode::BAD_REQUEST);
    register(&client, user).await.assert_status_is_ok();
    register(&client, user)
        .await
        .assert_status(StatusCode::CONFLICT);
}

async fn login(client: &TestClient<impl Endpoint>, name: &str, uuid: &str) -> TestResponse {
    client
        .post("/login")
        .content_type("application/x-www-form-urlencoded")
        .body(format!("name={}&uuid={}", name, uuid))
        .send()
        .await
}

#[tokio::test]
async fn test_login_logout() {
    let client = setup().await;

    let user = "alan";

    let register_resp = register(&client, user).await;
    register_resp.assert_status_is_ok();
    let user_uuid = register_resp.0.into_body().into_string().await.unwrap();

    let invalid_login_resp = login(&client, user, "invalid").await;
    invalid_login_resp.assert_status(StatusCode::BAD_REQUEST);

    let incorrect_uuid = Uuid::new_v4().to_string();
    let unauthorized_login_resp = login(&client, user, incorrect_uuid.as_str()).await;
    unauthorized_login_resp.assert_status(StatusCode::UNAUTHORIZED);

    let incorrect_name = "incorrect";
    let unauthorized_login_resp = login(&client, incorrect_name, user_uuid.as_str()).await;
    unauthorized_login_resp.assert_status(StatusCode::UNAUTHORIZED);

    let login_resp = login(&client, user, user_uuid.as_str()).await;
    login_resp.assert_status_is_ok();
    let cookie = login_resp.0.headers().get("set-cookie").unwrap();

    let logout_resp = client.post("/logout").header("cookie", cookie).send().await;
    logout_resp.assert_status_is_ok();
    logout_resp.assert_header_exist("set-cookie");

    let failed_logout_resp = client.post("/logout").header("cookie", cookie).send().await;
    failed_logout_resp.assert_status(StatusCode::UNAUTHORIZED);
}
