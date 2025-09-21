#[allow(dead_code)]
pub enum Value {
    Json(Box<Json>),
    Array(Vec<Json>),
    Number(i64),
    Float(f64),
    Boolean(bool),
    Null,
}

#[allow(dead_code)]
pub struct Json {
    key: String,
    value: Value,
}
