box::purge_cache()
box::use(
  httr2,
  dplyr,
  purrr,
  sf,
  spData
)

box::use(
  app/logic/oauth,
  app/logic/api,
  app/logic/wrangle
)

# Test workflow
url <- oauth$create_auth_url()
#browseURL(url)
# Get the return URL
parsed_url <- parseQueryString("http://127.0.0.1:6969/?state=&code=8d1e6980b43421a9f7c1c9c5fa4e33afad4c5491&scope=read,activity:read_all")
auth_code <- parsed_url$code
scope <- parsed_url$scope

auth_response <- oauth$token_exchange(auth_code)

token  <- auth_response$access_token
athlete <- auth_response$athlete
athlete_id <- athlete$id

# athlete
athlete_stats <- api$base_req(token) |>
  httr2$req_url_path_append(glue::glue("/athletes/{athlete_id}/stats")) |>
  httr2$req_perform() |>
  httr2$resp_body_json(flatten = TRUE)

# Activities ----------------------
act <- api$get_activities(year = 2023, token) |>
  dplyr$filter(grepl("Ride", type))

act_filt <- act |>
  dplyr$filter(!(trainer | type == "VirtualRide"))

## Card summaries
### trainer rides
trainer <- wrangle$trainer(act)

### Distance
distance <- wrangle$distance(act_filt)
### Elevation
elevation <- wrangle$elevation(act_filt)

### Time
time <- wrangle$time(act_filt)

## Fun
fun <- wrangle$fun(act_filt)

## By State
states <- wrangle$state_clean(act_filt)

wrangle$state_map(states)
