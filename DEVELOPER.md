# Developer notes

## Handling google authentication

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