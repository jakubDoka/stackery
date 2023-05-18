# Selecting

You can switch between search results with arrow keys and confirm selection with enter. If you rather prefer mouse you can use it too.

# Filtering

When searching posts you can click on author to only show posts from him. What may surprise you is that the button actually injects filter into your query. In case you dont want to click ut write it manually, here is the syntax:

```grammar
query  = { field } search
search = '.*'
field  = '\\' name ' ' value
name   = '[a-zA-Z_][a-zA-Z_0-9]*'
value  = '([^ ]*|"([^"]|\\")*")'
```

Further demonstration ('+' = name, '~' = value, '\*' = search):

```
\author "a b c" f
 ++++++ ~~~~~~~ *
\f a \g b c
 + ~  + ~ *
```
