Build
-----

- `make clean all`


Deploy
------

- Get a [Stateserver](https://github.com/speezepearson/stateserver) instance running. (You might need to use the `--unsafe` flag.)
- Edit `example-index.html` to point to that Stateserver instance.
- Edit `example-nginx.html` to reflect where you'll be hosting this.
- `make clean all`
- `rsync -havz dist/ me@my-domain-name.com:/usr/share/nginx/html/hanabi/`
- `rsync -hv example-index.html me@my-domain-name.com:/usr/share/nginx/html/hanabi/index.html`
- `rsync -hv example-nginx.conf me@my-domain-name.com:/etc/nginx/conf.d/hanabi.conf`

    Or, if you lack permissions, `rsync -hv example-nginx.conf me@my-domain-name.com:hanabi.conf && ssh me@my-domain-name.com -- 'sudo cp hanabi.conf /etc/nginx/conf.d/hanabi.conf'`

- `ssh me@my-domain-name.com -- 'mkdir -p states/ && tmux new -d -s "python -m stateserver -d states/"'`

