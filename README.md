# Latest

* **Status: beta.** Most of the functionality is in place,
more tests needed [![Build Status](https://travis-ci.org/dmitriid/unrest.png)](https://travis-ci.org/dmitriid/unrest)

* **[Milestone 0.1](https://github.com/dmitriid/unrest/issues/milestones?page=1&sort=completeness&state=closed) reached. Available [here](https://github.com/dmitriid/unrest/releases/tag/0.1)**

* Webmachine flow for [Milestone 0.2](https://github.com/dmitriid/unrest/issues/milestones?page=1&sort=completeness&state=open) is completed. Consult [``priv/webmachine.yml``](https://github.com/dmitriid/unrest/blob/master/priv/webmachine.yml)

* **Note**: master is kept stable (though might not point to a release). If you want latest
and greatest, grab the [in-progress branch](https://github.com/dmitriid/unrest/tree/in-progress)


# What?

Are you stuck with implementing the same functions over and over again for
Webmachine and can't find a flow for PATCH on
[their diagram](https://raw.github.com/wiki/basho/webmachine/images/http-headers-status-v3.png)?
Do you want to implement Andrei Neculau's [H^2 (Huge HTTP) Diagram](https://raw.github.com/andreineculau/http-decision-diagram/master/v4/http-decision-diagram-v4.png)
over Cowboy's [REST handler](http://ninenines.eu/docs/en/cowboy/HEAD/guide/rest_handlers)
but can't wrap your head around and can't extend it's execution flow?

Don't fret, _unrest_ is here to rescue you.

_unrest_ allows you to define your own execution flows, tailored to your needs.
If you're smart or lucky enough, you'll be able to control and change the behaviour
of your entire application just by moving lines around in a config file.

- use [``priv/config.yml``](https://github.com/dmitriid/unrest/blob/master/priv/config.yml) as a starting point for your own configs
- [``priv/webmachine.yml``](https://github.com/dmitriid/unrest/blob/master/priv/webmachine.yml) provides a **drop-in replacement for webmachine**. Consult the file for more details

# Gimme quickstart

1. Get rebar
2. `clone https://github.com/dmitriid/unrest && cd unrest`
3. `rebar get-deps`
4. `rebar compile`
5. `./start.sh` or `./start-debug.sh`
6. Go to [http://localhost:8080/](http://localhost:8080/) and see the debug
messages scroll by in the shell :)

The flows are defined in [priv/config.yml](https://github.com/dmitriid/unrest/blob/master/priv/config.yml).
A dummy handler for these flows is implemented in [src/unrest_service.erl](https://github.com/dmitriid/unrest/blob/master/src/unrest_service.erl)

## Can I use it from my app?

Yes. Include _unrest_ in your app and call
`unrest:start(path_to_config_file)`. You're set and ready to go.

What's a config file? Read on :)

## Full story

## Flows

The main concept behind _unrest_ is flows. A flow is just a chain of functions
that _unrest_ calls one by one and expects a certain kind of response before
proceeding to the next one.

This is a flow:

```erlang
%% A flow is [{M::atom(), F::atom()} | fun()]
[ {my_module, a_function_in_my_module}
, fun some_module:some_fun/1
, {my_module, some_other_fun}
, fun(Ctx) -> {ok, Ctx} end
]
```

You can use `unrest_flow:run/1,2` to run such a flow from within your code.

Note that each function in the flow will be called with _exactly one parameter_,
an _unrest context_.

## unrest context

An unrest context is the only way to interact between the functions in your
flow. You save your data to and retrieve it from the unrest context. See
[unrest_context.erl](blob/master/src/unrest_context.erl) to see what operations
on the context are available to you.

Besides data unrest context also stores the callstack of functions the flow has
executed, the errors that may have been generated during the run, etc. Let's see
now how all this comes together with the flow.

## Flow + context

As I mentioned above, the flow expects each function to return a specific
response and/or the updated context. These are the responses a function can
return:

- `{ok, unrest_context:context()}`

  This means that the function completed succesfully. The context it returned
  will be passed in to the next function.

- `{error, unrest_context:context(), ErrorData:any()}`

  Function generated a _non-critical_ error. The next function in chain will _still
  be called_. However, errors will be accumulated and will be accessible via:

  - `unrest_context:is_error_state/1` will return true
  - `unrest_context:errors/1` will return the error list

  This is useful when you run a chain of validating functions and you want to
  accumulate the list of errors

- `{stop_flow, unrest_context:context()}`

  Interrupt the flow without generating an error. Context returned from this
  function will be treated as the final value of the entire flow call

- `{stop_flow, unrest_context:context(), ErrorData:any()}`

  Interrupt the flow and generate the error. Context returned from this function
  will be treated as the final value of the entire flow call.

  - `unrest_context:is_error_state/1` will return true
  - `unrest_context:errors/1` will return the error list

- `{flow, FlowName:binary(), unrest_context:context()}`

  Run the named flow specified by the FlowName. A field named _flows_ must exist
  in the context and FlowName must exist in this proplist.

  Named flows are discussed below

- `{respond, Req::cowboy_req::req()}`

  Interrupt the flow and return response to the client. Use
  cowboy_req:reply/2,3,4 to produce Req.

  Normally, the last function in the flow would return this

## Routes and flows

Now that we have a way to define flows and know what returns o expect, we can
mix functions to create different flows for our web-application.

To do so we will use [YAML](http://www.yaml.org), Cowboy's
[matching syntax](http://ninenines.eu/docs/en/cowboy/HEAD/guide/routing) and
_unrest_'s flows.

On startup _unrest_ requires a configuration file that will define flows for all
the routes that exist in your application. Here's a simple example:

```yaml
/:
  GET:
    __flow__:
      root_module: init
      root_module: get
      root_module: output
```

This snippet reads as follows: Once we receive a `GET` request on `/`, call
`init`, `get` and `output` from `root_module`. `root_module:output/1` _must_
return `{respond, Req}`, and Req will be sent to the browser.

Easy, is it not? Let's change this a little. The output function is usually a libarary function that does conversion to JSON, for example. It would also be nice if we could skip writing the name of the module all the time.

```yaml
/:
  GET:
    __flow__:
      root_module:
        - init
        - check_headers
        - get
      output_module: to_json
  POST:
    __flow__:
      root_module:
        - init
        - check_headers
        - post
      output_module: to_json
```

Looks easy, doesn't it? We call three functions in `root_module`, then call
`output_module`. We also extended our config to include support for `POST`.

This, however, introduced a problem. We have to type the same `init` and
`check_headers` twice, once each for `GET` and `POST`. Since this is a common
part of the flow, can these be moved away in a flow of their own? Of course,
they can:

```yaml
/:
  GET:
    __flow__:
      __flow__: common_flow
      __flow__: auth_flow
      root_module:
        - get
      output_module: to_json
  POST:
    __flow__:
      __flow__:
        - common_flow
        - auth_flow
      root_module:
        - post
      output_module: to_json

__flows__:
   common_flow:
     root_module:
       - init
     common_lib:
       - check_content_type
       - check_accept
       - check_language
   auth_flow:
     auth_lib:
       - check_cookie
       - check_oauth2
```

Woah there, mister! What's going on?

Let's break this apart.

First, let's start with the `__flows__`. This configuration parameter defines
flows that can later be refered to by a name. In our case we define two flows:

- `common_flow` will call `root_module:init/1` and `common_lib:check_*`
  functions

- `auth_flow` will call `auth_lib:check_*` functions

The syntax for defining flows is the same as in the routing section above.

Now we need a way to call/include our flows. This is easy. Instead of specifying
a `module: function` call, you specify a `__flow__: flow_name`. So, in our case:

- When a call comes to `/`, and it's a `GET`, what is our flow?
  - call functions defined in `common_flow`
  - call functions defined in `auth_flow`
  - call `root_module:get/1`
  - call `output_module:to_json/1`
-  When a call comes to `/`, and it's a `POST`, what is our flow?
  - call functions defined in `common_flow`
  - call functions defined in `auth_flow`
  - call `root_module:post/1`
  - call `output_module:to_json/1`

See, easy as pie.

So, instead of writing 6 functions defined in `common_flow` and `auth_flow`
every time we need them, we write only two flows. But still... It means that we
have to write the same two flows each time we need them. Is there a way to make
a single flow that will call these two flows? Of course!

```yaml
/:
  GET:
    __flow__:
      __flow__: common_flow
      root_module: get
      output_module: to_json
  POST:
    __flow__:
      __flow__: common_flow
      root_module: post
      output_module: to_json

__flows__:
   common_flow:
     root_module:
       - init
     __flow__:
        - check_headers
        - check_auth
   check_headers:
     common_lib:
       - check_content_type
       - check_accept
       - check_language
   check_auth:
     auth_lib:
       - check_cookie
       - check_oauth2
```

This should be very easy by now. Using the same syntax as for routing, we just
include other flows in the definition of flows. So, `common_flow` will call all
the functions of `check_headers` flow and then all the functions of `check_auth`
flow.

And, of course, you can use `check_headers` and `check_auth` flows directly in
routing or in other flows as well.

This all leaves us with a small problem. We are going to call a `check_headers`
function, but how will it know what headers to check against? Well, besides
flows you can include other useful configuration parameters that will be passed
along to your function in the context:

```yaml
/:
  GET:
    headers:
        accept: application/json
    __flow__:
      __flow__: common_flow
      root_module: get
      output_module: to_json
  POST:
    headers:
        accept: application/json
        content-type: application/json
    __flow__:
      __flow__: common_flow
      root_module: post
      output_module: to_json

__flows__:
   common_flow:
     root_module:
       - init
     __flow__:
        - check_headers
        - check_auth
   check_headers:
     common_lib:
       - check_content_type
       - check_accept
       - check_language
   check_auth:
     auth_lib:
       - check_cookie
       - check_oauth2
```

And then in your function:

```erlang
check_headers(Context) ->
  Headers = unrest_context:get("headers", Context),
  Accept = proplists:get("accept", Headers)...
```

It's that easy!

Two things left to show though.

_First_: what if you want to skip all the flows and deal with Cowboy's requests
directly?

Easy! Just specify a module name instead of any config values:

```yaml
/:
  GET: index_handler
```

`index_handler` module will be responsible for providing all the callbacks
Cowboy requires.

_Second_: What about other routes?

Easy! Since _unrest_ runs on top of Cowboy, it lets you specify whatever routes
Cowboy accepts:

```yaml
/path/:with/[:optional/and/named/[:params]]:
  GET: index_handler
  POST:
    __flow__: common_flow
    root_module: post
    output_module:
        - to_json
        - add_headers
        - output
```

Refer to [Cowboy documentation](http://ninenines.eu/docs/en/cowboy/HEAD/guide/routing)
for more info.

# Technicalities

_unrest_ creates a dynamic dispatch for Cowboy that it generates from the config
file. The request passes through `unrest_middleware` which looks for
corresponding flows and runs them.

Given the following config:

```yaml
unrest configuration file
---

/:
  GET:
    headers:
        accept: application/json
    __flow__:
      __flow__: common_flow
      root_module: get
      output_module: to_json
  POST:
    headers:
        accept: application/json
        content-type: application/json
    __flow__:
      __flow__: common_flow
      root_module: post
      output_module: to_json

__flows__:
   common_flow:
     root_module:
       - init
     __flow__:
        - check_headers
        - check_auth
   check_headers:
     common_lib:
       - check_content_type
       - check_accept
       - check_language
   check_auth:
     auth_lib:
       - check_cookie
       - check_oauth2
```

Cowboy will receive the following dispatch:

```erlang
[{'_',[{"/",unrest_handler,
        [{config,[{<<"GET">>,
                   [{"headers",[{"accept","application/json"}]},
                    {<<"__flow__">>,
                     [{root_module,init},
                      {common_lib,check_content_type},
                      {common_lib,check_accept},
                      {common_lib,check_language},
                      {auth_lib,check_cookie},
                      {auth_lib,check_oauth2},
                      {root_module,get},
                      {output_module,to_json}]}]},
                  {<<"POST">>,
                   [{"headers",
                     [{"accept","application/json"},
                      {"content-type","application/json"}]},
                    {<<"__flow__">>,
                     [{root_module,init},
                      {common_lib,check_content_type},
                      {common_lib,check_accept},
                      {common_lib,check_language},
                      {auth_lib,check_cookie},
                      {auth_lib,check_oauth2},
                      {root_module,post},
                      {output_module,to_json}]}]}]},
         {flows,[{<<"common_flow">>,
                  [{root_module,init},
                   {common_lib,check_content_type},
                   {common_lib,check_accept},
                   {common_lib,check_language},
                   {auth_lib,check_cookie},
                   {auth_lib,check_oauth2}]},
                 {<<"check_headers">>,
                  [{common_lib,check_content_type},
                   {common_lib,check_accept},
                   {common_lib,check_language}]},
                 {<<"check_auth">>,
                  [{auth_lib,check_cookie},{auth_lib,check_oauth2}]}]}]}]}]
```
