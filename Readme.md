# LegalServerReaderHS

Haskell library and CLI for collecting data from the LegalServer Reports API.

**N.B. This project is not affiliated in any way with PSTechnologies**

## Installation

I've only built this to run on linux - if you're interested in other platforms, you can build it yourself with `stack build`, or let me know your use case.

Install from the releases page here on github. Just download the executable and put it somewhere on your `$PATH`.

## Usage

The LegalServer Reports API requires a set of credentials to use. There must be a _user_ account with permission to access the reports API. You must know the _password_ for the user.

You must also separately enable the Reports API for specific reports. See LegalServer's documetation for more information.

When you enable the api for a report, you need the _id_ of the report (a number, like 123) and the _apikey_ for accessing the report.

This library expects that you save these credentials in a `.ini` file that looks like the `example.ini`.

Once you've set up your `.ini` credentials file, to collect your report using the command line, use `lsreports`.

`lsreports` can give you basic instructions about how to use it:

```
$ lsreports --help
Client for using the LegalServer Reports API.

lsreports [OPTIONS]

Common flags:
  -c --configpath=ITEM    Path to api configuration file
  -r --report=ITEM        Name of the section in the configuration file
                          identifying the report to download
  -o --outputformat=ITEM  Format for output (only csv supported for now)
  -? --help               Display help message
  -V --version            Print version information

```

If I have created a config file based on `example.ini` called `config.ini`, then I can download a report configured in `config.ini`
and save it to a new file called `mydata.csv` with:

```
$ lsreports --configpath ./config.ini --report Intakes > mydata.csv
```
