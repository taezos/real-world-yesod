# real-world-yesod

## Database Setup
Start a development database
``` sh
docker-compose up db-dev
```
Run migrations on development database

``` sh
docker-compose up flyway-dev
```
Start a test database

``` sh
docker-compose up db-test
```
Run migrations on test database

``` sh
docker-compose up flyway-test
```

## Haskell Setup
Tools required:
* Stack
* ghcid
* yesod

### For nix users
Start a shell with
``` sh
nix-shell
```

## Development

Install yesod with:

``` sh
stack install yesod-bin --install-ghc
```

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

### Development with ghcid
To run type checking on `lib`

``` sh
make ghcid-lib
```

To run type checking on unit tests

``` sh
make ghcid-unit-test
```
## Interacting with the Application
Start a development server and go to `localhost:3000` or whatever is the`port` value in `config/settings.yml`
``` sh
yesod devel
```

## Tests
Run all tests
``` sh
stack test
```
Run unit tests

``` sh
stack test :unit
```
Run integrations tests

``` sh
stack test :integration
```

## Nix
Global stack configuration.

``` yaml
nix:
  enable: true
  packages: [ postgresql, zlib.dev, zlib.out ]
```
