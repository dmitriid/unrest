unrest
======

1. get rebar
2. rebar get-deps
3. rebar compile
4. ./start.sh or ./start-debug.sh
5. works via http://localhost:8080

Try
- http://localhost:8080/internal/errands/not_found
- http://localhost:8080/internal/errands/conflict
- http://localhost:8080/internal/errands/<any_id>

See what statuses are returned for those