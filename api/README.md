# server

## Setup
Install Homebrew/Linuxbrew, docker, docker-compose, and stack
`brew install docker`
`stack install ghcid`

In one terminal
`docker-compose up`

In a second terminal
`bin/server`

`curl localhost:7777/accounts`
which should ouput `[]` to your terminal.
