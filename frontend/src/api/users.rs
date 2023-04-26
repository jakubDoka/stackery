use std::fmt::Display;

use dioxus::core::IntoDynNode;

use crate::pages::{Name, Password};

use super::OTHER_MESSAGE;

crate::form_model!(RegisterModel { name: Name });

pub async fn register(model: RegisterModel) -> Result<RegisterResult, RegisterError> {
    let password = generate_password().ok_or(RegisterError::Other)?;

    let password_hash = hash_password(&password, &model.name).ok_or(RegisterError::Other)?;

    let form = bf_shared::api::user::RegisterForm {
        name: model.name.0,
        password_hash,
    };

    let response = super::CLIENT
        .post(crate::url!("users/"))
        .form(&form)
        .send()
        .await?;

    match response.status() {
        reqwest::StatusCode::OK => Ok(RegisterResult(password)),
        reqwest::StatusCode::BAD_REQUEST => Err(RegisterError::TooLong),
        reqwest::StatusCode::CONFLICT => Err(RegisterError::Conflict),
        _ => crate::reqwest_unexpected_status!(response, RegisterError),
    }
}

fn generate_password() -> Option<String> {
    let mut bytes = [0u8; 32];
    if let Err(e) = getrandom::getrandom(&mut bytes) {
        log::error!("getrandom error: {}", e);
        return None;
    }

    use base64::Engine;
    Some(base64::engine::general_purpose::STANDARD_NO_PAD.encode(&bytes))
}

fn hash_password(pass: &str, name: &str) -> Option<String> {
    let salt = bf_shared::api::user::load_password_salt(name);
    match bcrypt::hash_with_salt(pass, bcrypt::DEFAULT_COST, salt) {
        Ok(hash) => Some(hash.to_string()),
        Err(e) => {
            log::error!("bcrypt error: {}", e);
            None
        }
    }
}

pub struct RegisterResult(String);

impl Display for RegisterResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Your UUID is '{}'. Keep it safe in your password manager!",
            self.0
        )
    }
}

#[derive(Copy, Clone, Debug, thiserror::Error, PartialEq, Eq)]
pub enum RegisterError {
    #[error("name is too long")]
    TooLong,
    #[error("name is already taken")]
    Conflict,
    #[error("{OTHER_MESSAGE}")]
    Other,
}

crate::reqwest_error_handler!(RegisterError);

pub async fn login(model: LoginModel) -> Result<LoginResult, LoginError> {
    let password_hash = hash_password(&model.password, &model.name).ok_or(LoginError::Other)?;

    let form = bf_shared::api::user::LoginForm {
        name: model.name.0,
        password_hash,
    };

    let response = super::CLIENT
        .post(crate::url!("users/login"))
        .form(&form)
        .send()
        .await?;

    match response.status() {
        reqwest::StatusCode::OK => Ok(LoginResult),
        reqwest::StatusCode::UNAUTHORIZED => Err(LoginError::Unauthorized),
        _ => crate::reqwest_unexpected_status!(response, LoginError),
    }
}

crate::form_model!(LoginModel {
    name: Name,
    password: Password,
});

pub struct LoginResult;

impl Display for LoginResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "session will be active for next {:?}",
            bf_shared::db::session::DURATION
        )
    }
}

#[derive(Copy, Clone, Debug, thiserror::Error, PartialEq, Eq)]
pub enum LoginError {
    #[error("password or name is incorrect")]
    Unauthorized,
    #[error("{OTHER_MESSAGE}")]
    Other,
}

crate::reqwest_error_handler!(LoginError);

pub async fn logout() -> Result<(), LogoutError> {
    let response = super::CLIENT
        .post(crate::url!("users/logout"))
        .send()
        .await?;

    match response.status() {
        reqwest::StatusCode::OK => Ok(()),
        reqwest::StatusCode::UNAUTHORIZED => Err(LogoutError::Unauthorized),
        _ => crate::reqwest_unexpected_status!(response, LogoutError),
    }
}

#[derive(Copy, Clone, Debug, thiserror::Error, PartialEq, Eq)]
pub enum LogoutError {
    #[error("not even logged in")]
    Unauthorized,
    #[error("{OTHER_MESSAGE}")]
    Other,
}

crate::reqwest_error_handler!(LogoutError);

pub async fn search(query: String) -> Result<Vec<String>, SearchError> {
    let response = super::CLIENT
        .get(crate::url!("users/search?query={query}"))
        .send()
        .await?;

    match response.status() {
        reqwest::StatusCode::OK => Ok(response.json().await?),
        _ => crate::reqwest_unexpected_status!(response, SearchError),
    }
}

#[derive(Copy, Clone, Debug, thiserror::Error, PartialEq, Eq)]
pub enum SearchError {
    #[error("{OTHER_MESSAGE}")]
    Other,
}

impl<'a> IntoDynNode<'a> for SearchError {
    fn into_vnode(self, cx: &'a dioxus::prelude::ScopeState) -> dioxus::core::DynamicNode<'a> {
        cx.text_node(format_args!("search error: {}", self))
    }
}

crate::reqwest_error_handler!(SearchError);
