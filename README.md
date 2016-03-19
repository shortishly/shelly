# shelly

Shelly is an [erlang](http://www.erlang.org) OTP application that
makes it simple to have a remote shell into your application. Shelly
is designed to be embedded in applications to make it simple for teams
to provide operational support.

Running a SSH daemon in erlang is already very
[simple](http://erlang.org/doc/apps/ssh/using_ssh.html#id61601) - so
this is a general purpose micro application that you can include in
your OTP application enabling remote SSH directly into the
[BEAM](https://en.wikipedia.org/wiki/Erlang_(programming_language)).

Shelly uses the following environment variables:

|Variable           |Default      |Description                                              |
|-------------------|-------------|---------------------------------------------------------|
|SHELLY\_PORT       |22           |Shelly listenes for incoming SSH connections on this port|
|SHELLY\_ENABLED    |true         |Shelly is enabled when this value is true                |
|SHELLY\_SYSTEM\_DIR|priv/ssh     |Shelly uses this directory to find the host key          |
|SHELLY\_USER\_DIR  |priv/ssh     |Shelly uses this directory to find authorised_keys       |

Assuming that `~/.ssh` contains an `authorised_keys` file, typical usage:

```shell
SHELLY_USER_DIR=~/.ssh SHELLY_PORT=22022 make shell
```

In another shell, you can log into the BEAM as follows:

```shell
ssh -p 22022 localhost
Eshell V7.3  (abort with ^G)
(shelly@Office-iMac)1>
```
