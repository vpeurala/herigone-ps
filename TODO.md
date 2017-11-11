# TODO

* Use docker-compose, Kubernetes or something similar to take care of the dependencies between submodules. The current shell-script-based approach is a little fragile.
* Learn purescript-halogen and use it on the client side.
* Move the flyway stuff out from under purescript-herigone-server; it might be better to have it as an independent submodule, or as a part of purescript-herigone-db.
* Add all the basic association data to flyway migrations, you can get it easily from https://github.com/vpeurala/herigone-elm project.
