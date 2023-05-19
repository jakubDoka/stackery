use futures_util::stream::{StreamExt, TryStreamExt};
use mongodb::bson::doc;
use mongodb::IndexModel;
use poem::session::Session;
use poem_openapi::param::Path;
use poem_openapi::payload::PlainText;
use poem_openapi::{param::Query, payload::Json};

use crate::SearcherClient;

use bf_shared::*;

use super::users::Sessions;

#[cfg(test)]
mod test;

pub struct Posts {
    db: mongodb::Database,
    searcher: SearcherClient<bf_shared::search::post::Model>,
    sessions: Sessions,
}

#[poem_openapi::OpenApi]
impl Posts {
    pub async fn new(
        db: mongodb::Database,
        searcher: SearcherClient<bf_shared::search::post::Model>,
        sessions: Sessions,
    ) -> Self {
        Self::create_indexes(&db).await;
        Self {
            db,
            searcher,
            sessions,
        }
    }

    async fn create_indexes(db: &mongodb::Database) {
        db.collection::<()>(db::post::COLLECTION)
            .create_index(
                IndexModel::builder()
                    .keys(doc! { db::post::AUTHOR: 1 })
                    .build(),
                None,
            )
            .await
            .unwrap();
    }

    #[oai(method = "post", path = "/")]
    async fn create(&self, session: &Session, post: Json<api::post::Model>) -> CreateResponse {
        let session = crate::auth!(self, session, CreateResponse);

        let Json(api::post::Model { name, code }) = post;

        #[derive(serde::Deserialize)]
        struct Post {}
        let query = doc! { db::post::NAME: name.as_str() };
        let existing_post = http_try!(self.posts::<Post>().find_one(query, None).await,
            CreateResponse::InternalServerError,
            error "failed to select post");

        if existing_post.is_some() {
            return CreateResponse::Conflict;
        }

        if let Err(e) = db::validate_name(&name) {
            crate::log!(trace, "invalid post name"; "NAME" => name.as_str(), "ERROR" => e.to_string());
            return CreateResponse::BadRequest;
        }

        if let Err(e) = db::post::validate_code(&code) {
            crate::log!(trace, "invalid post code"; "CODE" => code.as_str(), "ERROR" => e.to_string());
            return CreateResponse::BadRequest;
        }

        let new_post = doc! {
            db::post::NAME: name.as_str(),
            db::post::CODE: code.as_str(),
            db::post::AUTHOR: session.name.as_str(),
        };

        http_try!(self.posts().insert_one(new_post, None).await,
            CreateResponse::InternalServerError,
            error "failed to insert post");

        let search_post = bf_shared::search::post::Model {
            name,
            author: session.name,
            code,
        };

        http_try!(self.searcher.add(search_post).await,
            CreateResponse::InternalServerError,
            error "failed to create post in search");

        CreateResponse::Created
    }

    #[oai(method = "get", path = "/:id/code")]
    async fn get_code(&self, id: Path<String>) -> GetCodeResponse {
        #[derive(serde::Deserialize)]
        struct Post {
            code: String,
        }

        let query = doc! { db::post::NAME: id.as_str() };
        let post = http_try!(self.posts::<Post>().find_one(query, None).await,
            GetCodeResponse::InternalServerError,
            error "failed to select post");

        let post = match post {
            Some(post) => post,
            None => return GetCodeResponse::NotFound,
        };

        GetCodeResponse::Ok(PlainText(post.code))
    }

    #[oai(method = "get", path = "/search")]
    async fn search(&self, query: Query<String>) -> SearchResponse {
        let Query(query) = query;

        if query.trim().is_empty() {
            return SearchResponse::Ok(Json(Vec::new()));
        }

        let (filter, query) = 'a: {
            let mut parser = bf_shared::search::Query::new(query.as_str());

            let Some(author) = parser.get_field("author") else {
                break 'a ("".to_string(), query.as_str());
            };

            parser.by_ref().count(); // strip fields

            let author = author.trim_matches('"');

            (format!("author = '{author}'"), parser.reminder())
        };

        let search_result = http_try!(self.searcher.search_with_filter(query, &filter).await,
            SearchResponse::InternalServerError,
            error "failed to search posts");

        SearchResponse::Ok(Json(search_result))
    }

    #[oai(method = "get", path = "/for-user/:user_name")]
    async fn for_user(&self, user_name: Path<String>) -> SearchResponse {
        let query = doc! { db::post::AUTHOR: user_name.as_str() };

        let cursor = http_try!(self.posts::<bf_shared::db::post::Model>().find(query, None).await,
            SearchResponse::InternalServerError,
            error "failed to select posts");

        let posts = http_try!(cursor.map(|r| r.map(bf_shared::search::post::Model::from)).try_collect::<Vec<_>>().await,
            SearchResponse::InternalServerError,
            error "failed to collect posts");

        SearchResponse::Ok(Json(posts))
    }

    fn posts<T>(&self) -> mongodb::Collection<T> {
        self.db.collection::<T>(db::post::COLLECTION)
    }
}

#[derive(poem_openapi::ApiResponse)]
enum CreateResponse {
    #[oai(status = 201)]
    Created,
    // the post name or code is invalid
    #[oai(status = 400)]
    BadRequest,
    // post name is not unique
    #[oai(status = 409)]
    Conflict,
    // the user is not logged in
    #[oai(status = 401)]
    Unauthorized,
    #[oai(status = 500)]
    InternalServerError,
}

#[derive(poem_openapi::ApiResponse)]
enum GetCodeResponse {
    #[oai(status = 200)]
    Ok(PlainText<String>),
    #[oai(status = 404)]
    NotFound,
    #[oai(status = 500)]
    InternalServerError,
}

#[derive(poem_openapi::ApiResponse)]
enum SearchResponse {
    #[oai(status = 200)]
    Ok(Json<Vec<bf_shared::search::post::Model>>),
    #[oai(status = 500)]
    InternalServerError,
}
