# LegalServerReaderHS

Haskell library and CLI for collecting data from the LegalServer Reports API.

**N.B. This project is not affiliated in any way with PSTechnologies**

## Usage

The LegalServer Reports API requires a set of credentials to use. There must be a _user_ account with permission to access the reports API. You must know the _password_ for the user.

You must also separately enable the Reports API for specific reports. See LegalServer's documetation for more information.

When you enable the api for a report, you need the _id_ of the report (a number, like 123) and the _apikey_ for accessing the report.

This library expects that you save these credentials in a `.ini` file that looks like the `example.ini`.

Once you've set up your `.ini` credentials file, to collect your report using the command line, use

```

```
