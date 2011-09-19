{application, sockjs,
 [{description, "SockJS"},
  {vsn, "0.01"},
  {modules, []},
  {registered, []},
  {mod, {sockjs_app, []}},
  {env, [{sockjs_url, "/lib/sockjs.js"}]},
  {applications, [kernel, stdlib]}]}.
