use std::{collections::HashMap, hash::BuildHasher};

use super::OTHER_MESSAGE;

pub async fn register(
    values: &HashMap<String, String, impl BuildHasher>,
) -> Result<String, RegisterError> {
    let response = super::CLIENT
        .post(crate::url!("users/register"))
        .form(values)
        .send()
        .await?;

    match response.status() {
        reqwest::StatusCode::OK => Ok(response.text().await?),
        reqwest::StatusCode::BAD_REQUEST => Err(RegisterError::TooLong),
        reqwest::StatusCode::CONFLICT => Err(RegisterError::Conflict),
        _ => crate::reqwest_unexpected_status!(response, RegisterError),
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

pub async fn login(values: &HashMap<String, String, impl BuildHasher>) -> Result<(), LoginError> {
    let response = super::CLIENT
        .post(crate::url!("users/login"))
        .form(values)
        .send()
        .await?;

    match response.status() {
        reqwest::StatusCode::OK => Ok(()),
        reqwest::StatusCode::BAD_REQUEST => Err(LoginError::Invalid),
        reqwest::StatusCode::UNAUTHORIZED => Err(LoginError::Unauthorized),
        _ => crate::reqwest_unexpected_status!(response, LoginError),
    }
}

#[derive(Copy, Clone, Debug, thiserror::Error, PartialEq, Eq)]
pub enum LoginError {
    #[error("invalid name or uuid")]
    Invalid,
    #[error("password or uuid is incorrect")]
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

crate::reqwest_error_handler!(SearchError);
