box::use(
  httr2,
  config
)

client_id <- Sys.getenv("STRAVA_CLIENT_ID")
client_secret <- Sys.getenv("STRAVA_CLIENT_SECRET")

#' Strava client specifications. IE my app that is registered
strava_client <- function() {
  httr2$oauth_client(
    id = client_id,
    secret = client_secret,
    token_url = "https://www.strava.com/oauth/token",
    name = "r-Strava-test"
  )
}

#' Generate authorization URL, sends the user to strava's oauth to grant
#' my app access
#' @export
create_auth_url <- function() {
  httr2$oauth_flow_auth_code_url(
    client = strava_client(),
    auth_url = "https://www.strava.com/oauth/authorize",
    redirect_uri =  config$get("redirect_uri"),
    scope = "read,activity:read_all"
  )
}

#' Perform token handshake in exchange for a bearer and refresh token. Strava
#' Also sends back the athlete profile.
#' @export
token_exchange <- function(auth_code) {
  # Build the post request
  req <- httr2$request("https://www.strava.com/oauth/token") |>
    httr2$req_method("POST") |>
    httr2$req_body_form(
      client_id = client_id,
      client_secret = client_secret,
      code = auth_code,
      grant_type = "authorization_code"
    )

  resp <- req |>
    httr2$req_perform()

  resp_body <- resp |>
    httr2$resp_body_json()

  return(resp_body)
}


