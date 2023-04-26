use std::fmt;

use reqwest::StatusCode;

use crate::pages::{Code, Name};

pub async fn create(post: CreateModel) -> Result<CreateResult, CreateError> {
    let response = super::CLIENT
        .post(crate::url!("posts"))
        .json(&post)
        .send()
        .await?;

    match response.status() {
        StatusCode::CREATED => Ok(CreateResult),
        StatusCode::BAD_REQUEST => Err(CreateError::InvalidNameOrCode),
        StatusCode::CONFLICT => Err(CreateError::NameTaken),
        StatusCode::UNAUTHORIZED => Err(CreateError::NotLoggedIn),
        _ => crate::reqwest_unexpected_status!(response, CreateError),
    }
}

crate::form_model!(CreateModel {
    name: Name,
    code: Code,
});

pub struct CreateResult;

impl fmt::Display for CreateResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "post created")
    }
}

#[derive(thiserror::Error, Debug)]
pub enum CreateError {
    #[error("name or code is invalid")]
    InvalidNameOrCode,
    #[error("name is already taken")]
    NameTaken,
    #[error("you need to login to post")]
    NotLoggedIn,
    #[error("{}", super::OTHER_MESSAGE)]
    Other,
}

crate::reqwest_error_handler!(CreateError);

pub(crate) async fn get_code(id: &str) -> Result<String, GetCodeError> {
    let response = super::CLIENT
        .get(crate::url!("posts/{}/code", id))
        .send()
        .await?;

    match response.status() {
        StatusCode::OK => Ok(response.text().await?),
        StatusCode::NOT_FOUND => Err(GetCodeError::NotFound),
        _ => crate::reqwest_unexpected_status!(response, GetCodeError),
    }
}

#[derive(thiserror::Error, Debug)]
pub enum GetCodeError {
    #[error("post not found")]
    NotFound,
    #[error("{}", super::OTHER_MESSAGE)]
    Other,
}

crate::reqwest_error_handler!(GetCodeError);

pub async fn search(query: String) -> Result<Vec<bf_shared::search::post::Model>, SearchError> {
    let response = super::CLIENT
        .get(crate::url!("posts/search"))
        .query(&[("query", query)])
        .send()
        .await?;

    match response.status() {
        StatusCode::OK => Ok(response.json().await?),
        _ => crate::reqwest_unexpected_status!(response, SearchError),
    }
}

#[derive(thiserror::Error, Debug)]
pub enum SearchError {
    #[error("{}", super::OTHER_MESSAGE)]
    Other,
}

crate::reqwest_error_handler!(SearchError);
