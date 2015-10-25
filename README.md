#yammat

Yet Another MateMAT

##Introduction

This project aims to be an implementation for a trust based POS for hackerspaces.

##Dependencies

###External dependencies

In order to build yammat you need the packages `cabal-install`, `ghc`, `libmagick++-dev` and `postgresql`. Furthermore you need the Haskell packages `alex` and `happy`. To install these enter `cabal install alex happy` into your command line.

###Internal dependencies

Before installing the internal dependencies it is best practice to put the project into a sandbox with `cabal sandbox init`.

All internal dependencies are in the file `yammat.cabal` and will be installed automagically with the command `cabal install --only-dependencies`.

##Building

To build this project enter `cabal configure && cabal build exe:yammat` into your command line

##Deployment

Copy or link the executable `dist/build/yammat/yammat` to your desired run location alongside with the folders `static` and `config`. The Folders should be copied, or you will get problems with your git pulls.

##Configuration

Check the configuration File `config/settings.yml`. Create Postgresql Databases according to these settings.

##Lift-Off

Run `./yammat config/settings.yml` in your desired run location. Finally point a reverse-proxy (something like nginx) at `http://localhost:3000` or any other port you configured in `config/settings.yml`.

For better control You can wrap an init script around this. How to do this is described [in my blog][blog].

[blog]: http://nek0.eu/posts/2014-10-23-Daemonize-a-yesod-app.html

##Migrations

###0.0.0-0.0.1

* Delete column `alt_time` from table `avatar` in your Database with `alter table "avatar" drop column "alt_time";`
* Start yammat normally to fill database with dummy data and stop it again
* Run migration script
	* if you have built yammat with a sandbox, run `runghc -package-db/full/path/to/sandbox(XXX-ghc-version-packages.conf.d /path/to/yammat/Migration/0.0.0-0.0.1/Migration.hs`
		* Note: No space between the option `-package-db` and its argument
	* without sandbox: `runghc /path/to/yammat/Migration/0.0.0-0.0.1/Migration.hs`
* Enjoy your freshly migrated Matemat
