use mockall::predicate;
use poem::{
    http::{HeaderValue, StatusCode},
    session::CookieSession,
    test::{TestClient, TestResponse},
    Endpoint, EndpointExt, Route,
};
use poem_openapi::OpenApiService;
use uuid::Uuid;

use crate::{db, search_client::SearchUser, SearcherClient};

pub async fn setup(searcher: impl Into<Option<SearcherClient>>) -> TestClient<impl Endpoint> {
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

    let users = OpenApiService::new(
        super::Users::new(db, searcher.into().unwrap_or_default()).await,
        "users",
        "1.0.0",
    );
    TestClient::new(
        Route::new()
            .nest("/", users)
            .with(CookieSession::new(super::cookie_config())),
    )
}

async fn register(client: &TestClient<impl Endpoint>, name: &str) -> TestResponse {
    client
        .post("/register")
        .form(&[("name", name)])
        .send()
        .await
}

#[tokio::test]
async fn test_register() {
    let user = "alec";

    let mut searcher = SearcherClient::default();
    searcher
        .expect_add_user()
        .with(predicate::eq(user))
        .returning(|_| Ok(()));
    let client = setup(searcher).await;

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
        .form(&[("name", name), ("uuid", uuid)])
        .send()
        .await
}

#[tokio::test]
async fn test_login_logout() {
    let user = "alan";

    let mut searcher = SearcherClient::default();
    searcher
        .expect_add_user()
        .with(predicate::eq(user))
        .returning(|_| Ok(()));
    let client = setup(searcher).await;

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

async fn delete(client: &TestClient<impl Endpoint>, cookie: &HeaderValue) -> TestResponse {
    client
        .delete("/delete")
        .header("cookie", cookie)
        .send()
        .await
}

#[tokio::test]
async fn test_delete() {
    let user = "bob";

    let mut searcher = SearcherClient::default();
    searcher
        .expect_add_user()
        .with(predicate::eq(user))
        .returning(|_| Ok(()));
    searcher
        .expect_delete_user()
        .with(predicate::eq(user))
        .returning(|_| Ok(()));

    let client = setup(searcher).await;

    let register_resp = register(&client, user).await;
    register_resp.assert_status_is_ok();

    let user_uuid = register_resp.0.into_body().into_string().await.unwrap();
    let login_resp = login(&client, user, user_uuid.as_str()).await;
    let cookie = login_resp.0.headers().get("set-cookie").unwrap();

    let delete_resp = delete(&client, cookie).await;
    delete_resp.assert_status_is_ok();

    let failed_delete_resp = delete(&client, cookie).await;
    failed_delete_resp.assert_status(StatusCode::UNAUTHORIZED);

    let failed_login_resp = login(&client, user, user_uuid.as_str()).await;
    failed_login_resp.assert_status(StatusCode::UNAUTHORIZED);
}
