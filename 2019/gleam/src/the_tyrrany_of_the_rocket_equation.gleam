pub external type Whatevs
external fn put_chars(a) -> Whatevs = "io" "put_chars"

pub fn main(_) {
  put_chars("Hello, World!\n")
}