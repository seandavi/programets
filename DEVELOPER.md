# Developer notes

## Development setup

Add the following to your `.Renviron` file:

```
GARGLE_ENCRYPTION_KEY = "your_key_here"
```
This will set the `GARGLE_ENCRYPTION_KEY` environment variable, which is used to encrypt and decrypt the google service account json file.


## Encrypting the google service account json file

This section just describes how we set up the service account file and encryption. We need do this only once. The google service account json file contains sensitive information, such as the private key, which should not be stored in the repository. To protect this information, we encrypt the json file using the `gargle` package.

We follow the documentation at https://gargle.r-lib.org/articles/managing-tokens-securely.html to encrypt the google service account json file.

Step 1: Create a key for encryption

```{r}
library(gargle)
key = gargle::secret_make_key()
key
```

Step 2: Store the key in an environment variable

Store the key in an environment variable. You can do this by adding the environment variable to your `.Renviron` file. To open the file, `usethis::edit_r_environ()`.

Add the following line to the file:

```
GARGLE_ENCRYPTION_KEY = "your_key_here"
```

Step 3: Encrypt the google service account json file

```{r}
gargle::secret_encrypt_json(
  path = "path/to/ga4-acess-keyfile.json",
  key = "GARGLE_ENCRYPTION_KEY",
  output = "inst/secret/ga4-acess-keyfile.json"
)
```
This will create an encrypted version of the json file in the `inst/secret` directory.

Step 4: Use the encrypted file in your code

```{r}
library(gargle)
googleAnalyticsR::ga_auth(
  json_file = gargle::secret_decrypt_json(
    path = system.file("secret/ga4-acess-keyfile.json", package = "programets"),
    key = "GARGLE_ENCRYPTION_KEY"
  )
)
```