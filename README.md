# yammat

Yet Another MateMAT

## Introduction

This project aims to be an implementation for a trust based POS for hackerspaces.

## Installation

### Dependencies

#### Environment dependencies

A working Haskell capable environment. For that you will need `haskell-stack`, which can be installed
with:

```bash
sudo apt-get install haskell-stack
```

In case you can't find this package in your repositories, get `stack` from [stackage][stackage].

After you installed `stack`, let it make itself comfortable in your system with

```bash
stack setup
```

This might, depending on your system setup, take some time. Brew yourself some tea.

#### Build dependencies

Now that your Haskell environment is set up, you need to install the dependencies for building and running yammat, which are:

* alex
* happy
* libpq-dev
* postgresql

Install all of them through your package management system.

### Building

To build this project enter `stack build` into your command line.
Everything else should be done automagically.

This will take a long time, so go on a quest to find some cookies to go with the tea.

## Deployment

Create a directory outside of the project directory, where you want to actually run the application.
Copy or link the executable `yammat` from `.stack-work/dist/<arch>/Cabal-<version>/build/eidolon/`,
where `<arch>` is your systems architecture and `<version>` the version of your `cabal` library,
to your desired run location alongside with the folders `static` and `config` and their contenst.
The Folders should be copied, or you will get problems with your git pulls.

## Configuration

Let's leave the project directory and enter your desired run location.
Check the configuration File `config/settings.yml` and alter its content to your liking.
Most of these settings normally don't need to be altered much, except for `approot`.
Change this setting to the root address with which you want to connect to the application,
including the scheme (http or https).
Additionally edit the settings `email`, `currency` and `cash_charge`.

* `email` is the email address, which will receive notifications from yammat, should the stock run low.  
* `currency` is your currency sign or shorthand, which will be used when displaying prices.
* `cash_charge` is the extra you want to charge your users, who pay cash.
	* *Note:* The value of this setting is in hundredths of your currency denomination.

`cash_charge` is effectively a "guest tax". Setting it to `0` is perfectly fine, if you don't want that.

Create a Postgresql User and Database according to the settings in the settings file
and grant the user all privileges on the database.

## Lift-Off

Run `./yammat config/settings.yml` in your desired run location. Finally point a reverse-proxy
(something like nginx) at `http://localhost:3000` or any other port you configured in
`config/settings.yml`.

For better control You can wrap an systemd unit file around this.
How to do this is described [in my blog][blog].

## Migrations

### 0.0.0-0.0.1

* Delete column `alt_time` from table `avatar` in your Database with
  `alter table "avatar" drop column "alt_time";`
* Start yammat normally to fill database with dummy data and stop it again
* Run migration script
	* if you have built yammat with a sandbox, run
`runghc -package-db/full/path/to/sandbox(XXX-ghc-version-packages.conf.d
/path/to/yammat/Migration/0.0.0-0.0.1/Migration.hs`
		* Note: No space between the option `-package-db` and its argument
	* without sandbox: `runghc /path/to/yammat/Migration/0.0.0-0.0.1/Migration.hs`
* Enjoy your freshly migrated Matemat

### 0.0.2-0.0.3

* stop old matemat
* create a view with new and old timestamps and user ids with
  `create or replace view "user_new" as select "user".id, "user".timestamp, date 'epoch' + "user".timestamp * interval '1 second' as timestamp_new from "user";
* create temporary timestamp column in user table with
  `alter table "user" create column "timestamp_temp";`
* fill temporary timestamp column with new timestamps with
  `update "user" set timestamp_temp = (select timestamp_new from from user_new where user_new.id = "user".id)`
* check if new timestamps look sane
* drop old timestamp column
  `alter table "user" drop column timestamp cascade;`
* rename temporary column
  `alter table "user" rename column timestamp_temp to "timestamp";`
* start new matemat

[stackage]: http://www.stackage.org/
[blog]: https://nek0.eu/posts/2015-08-28-Daemonize-a-Yesod-application-systemd-style.html
