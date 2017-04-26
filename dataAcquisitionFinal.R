library(shiny)
library(httr)
library(dplyr)
library(ggplot2)


circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data.frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

theme_court = function(base_size = 16) {
  theme_bw(base_size) +
    theme(
      text = element_text(color = "#f0f0f0"),
      plot.background = element_rect(fill = bg_color, color = bg_color),
      panel.background = element_rect(fill = bg_color, color = bg_color),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks.length = unit(0, "lines"),
      legend.background = element_rect(fill = bg_color, color = bg_color),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

short_three_radius = 22
short_three_seasons = c("1994-95", "1995-96", "1996-97")

court_points = data.frame(
  x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
  y = c(height, 0, 0, height, height),
  desc = "perimeter"
)

court_points = rbind(court_points , data.frame(
  x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
  y = c(0, key_height, key_height, 0),
  desc = "outer_key"
))

court_points = rbind(court_points , data.frame(
  x = c(-backboard_width / 2, backboard_width / 2),
  y = c(backboard_offset, backboard_offset),
  desc = "backboard"
))

court_points = rbind(court_points , data.frame(
  x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
))

foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
foul_circle_top = filter(foul_circle, y > key_height) %>% mutate(desc = "foul_circle_top")
foul_circle_bottom = filter(foul_circle, y < key_height) %>% mutate(desc = "foul_circle_bottom")

hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>% mutate(desc = "hoop")

restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
  filter(y >= hoop_center_y) %>%
  mutate(desc = "restricted")

three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>% filter(y >= three_point_side_height)
short_three_circle = circle_points(center = c(0, hoop_center_y), radius = short_three_radius) %>% filter(y >= hoop_center_y)

three_point_line = data.frame(
  x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
  y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
  desc = "three_point_line"
)

short_three_line = data.frame(
  x = c(three_point_side_radius, three_point_side_radius, short_three_circle$x, -three_point_side_radius, -three_point_side_radius),
  y = c(0, hoop_center_y, short_three_circle$y, hoop_center_y, 0),
  desc = "short_three_line"
)

court_without_three = rbind(court_points , foul_circle_top, foul_circle_bottom, hoop, restricted)

court_points = rbind(court_without_three, three_point_line)
court_points = mutate(court_points , dash = (desc == "foul_circle_bottom"))

short_three_court_points = rbind(court_without_three, short_three_line)
short_three_court_points = mutate(short_three_court_points , dash = (desc == "foul_circle_bottom"))

court = ggplot() +
  geom_path(data = court_points,
            aes(x = x, y = y, group = desc, linetype = dash),
            color = "#999999") +
  scale_linetype_manual(values = c("solid", "longdash"), guide = FALSE) +
  coord_fixed(ylim = c(0, 35), xlim = c(-25, 25)) +
  theme_court(base_size = 22)

short_three_court = ggplot() +
  geom_path(data = short_three_court_points,
            aes(x = x, y = y, group = desc, linetype = dash),
            color = "#999999") +
  scale_linetype_manual(values = c("solid", "longdash"), guide = FALSE) +
  coord_fixed(ylim = c(0, 35), xlim = c(-25, 25)) +
  theme_court(base_size = 22)





#======================================================================================================


fetch_shots_by_player_id_and_season = function(player_id, season) {
  req(player_id, season)
  
  request = GET(
    "http://stats.nba.com/stats/shotchartdetail",
    query = list(
      PlayerID = player_id,
      PlayerPosition = "",
      Season = season,
      ContextMeasure = "FGA",
      DateFrom = "",
      DateTo = "",
      GameID = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      Period = 0,
      Position = "",
      RookieYear = "",
      SeasonSegment = "",
      SeasonType = "Regular Season",
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    ),
    user_agent("mac_safari")
  )
  
  stop_for_status(request)
  
  data = content(request)
  
  raw_shots_data = data$resultSets[[1]]$rowSet
  col_names = tolower(as.character(data$resultSets[[1]]$headers))
  
  if (length(raw_shots_data) == 0) {
    shots = data.frame(
      matrix(nrow = 0, ncol = length(col_names))
    )
  } else {
    shots = data.frame(
      matrix(
        unlist(raw_shots_data),
        ncol = length(col_names),
        byrow = TRUE
      )
    )
  }
  
  shots = tbl_df(shots)
  names(shots) = col_names
  
  player_id = as.numeric(player_id)
  cbind(shots, player_id)
  
  shots = mutate(shots,
                 loc_x = as.numeric(as.character(loc_x)) / 10,
                 loc_y = as.numeric(as.character(loc_y)) / 10 + hoop_center_y,
                 shot_distance = as.numeric(as.character(shot_distance)),
                 shot_made_numeric = as.numeric(as.character(shot_made_flag)),
                 shot_made_flag = factor(shot_made_flag, levels = c("1", "0"), labels = c("made", "missed")),
                 shot_attempted_flag = as.numeric(as.character(shot_attempted_flag)),
                 shot_value = ifelse(tolower(shot_type) == "3pt field goal", 3, 2)
                 
  )
  
  raw_league_avg_data = data$resultSets[[2]]$rowSet
  league_avg_names = tolower(as.character(data$resultSets[[2]]$headers))
  league_averages = tbl_df(data.frame(
    matrix(unlist(raw_league_avg_data), ncol = length(league_avg_names), byrow = TRUE)
  ))
  names(league_averages) = league_avg_names
  league_averages = mutate(league_averages,
                           fga = as.numeric(as.character(fga)),
                           fgm = as.numeric(as.character(fgm)),
                           fg_pct = as.numeric(as.character(fg_pct)),
                           shot_value = ifelse(shot_zone_basic %in% c("Above the Break 3", "Backcourt", "Left Corner 3", "Right Corner 3"), 3, 2)
  )
  
  return(list(player = shots, league_averages = league_averages))
}

default_shots = (fetch_shots_by_player_id_and_season(201146, "2007-08"))

JamesLebron = data.frame(default_shots$player)
JamesLebron_part2 = data.frame(default_shots$league_averages)

write.csv(JamesLebron, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Yijianlian07-08.csv")
write.csv(JamesLebron_part2, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Yijianlian07-08_part2.csv")

marcusMorris = fetch_shots_by_player_id_and_season(202694, "2015-16")
marcusMorris = data.frame(marcusMorris$player)
write.csv(marcusMorris, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/marcusMorris.csv")

ersanIlyasova = fetch_shots_by_player_id_and_season(101141, "2015-16")
ersanIlyasova = data.frame(ersanIlyasova$player)
write.csv(ersanIlyasova, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/ersanIlyasova.csv")

andreDrummond = fetch_shots_by_player_id_and_season(203083, "2015-16")
andreDrummond = data.frame(andreDrummond$player)
write.csv(andreDrummond, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/andreDrummond.csv")

kentaviousCaldwellPope = fetch_shots_by_player_id_and_season(203484, "2015-16")
kentaviousCaldwellPope = data.frame(kentaviousCaldwellPope$player)
write.csv(kentaviousCaldwellPope, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/kentaviousCaldwellPope.csv")

reggieJackson = fetch_shots_by_player_id_and_season(202704, "2015-16")
reggieJackson = data.frame(reggieJackson$player)
write.csv(reggieJackson, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/reggieJackson.csv")

stanleyJohnson = fetch_shots_by_player_id_and_season(1626169, "2015-16")
stanleyJohnson = data.frame(stanleyJohnson$player)
write.csv(stanleyJohnson, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/stanleyJohnson.csv")

stanleyJohnson = fetch_shots_by_player_id_and_season(1626169, "2015-16")
stanleyJohnson = data.frame(stanleyJohnson$player)
write.csv(stanleyJohnson, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/stanleyJohnson.csv")

aronBaynes = fetch_shots_by_player_id_and_season(203382, "2015-16")
aronBaynes = data.frame(aronBaynes$player)
write.csv(aronBaynes, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/aronBaynes.csv")

steveBlake = fetch_shots_by_player_id_and_season(2581, "2015-16")
steveBlake = data.frame(steveBlake$player)
write.csv(steveBlake, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/steveBlake.csv")

jodieMeeks = fetch_shots_by_player_id_and_season(201975, "2015-16")
jodieMeeks = data.frame(jodieMeeks$player)
write.csv(jodieMeeks, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/jodieMeeks.csv")
#====================================================================================
#====================================================================================
kentBazemore = fetch_shots_by_player_id_and_season(203145, "2015-16")
kentBazemore = data.frame(kentBazemore$player)
write.csv(kentBazemore, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/kentBazemore.csv")

paulMillsap = fetch_shots_by_player_id_and_season(200794, "2015-16")
paulMillsap = data.frame(paulMillsap$player)
write.csv(paulMillsap, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/paulMillsap.csv")

alHorford = fetch_shots_by_player_id_and_season(201143, "2015-16")
alHorford = data.frame(alHorford$player)
write.csv(alHorford, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/alHorford.csv")

kyleKorver = fetch_shots_by_player_id_and_season(2594, "2015-16")
kyleKorver = data.frame(kyleKorver$player)
write.csv(kyleKorver, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/kyleKorver.csv")

jeffTeague = fetch_shots_by_player_id_and_season(201952, "2015-16")
jeffTeague = data.frame(jeffTeague$player)
write.csv(jeffTeague, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/jeffTeague.csv")

thaboSefolosha = fetch_shots_by_player_id_and_season(200757, "2015-16")
thaboSefolosha = data.frame(thaboSefolosha$player)
write.csv(thaboSefolosha, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/thaboSefolosha.csv")

tiagoSplitter = fetch_shots_by_player_id_and_season(201168, "2015-16")
tiagoSplitter = data.frame(tiagoSplitter$player)
write.csv(tiagoSplitter, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/tiagoSplitter.csv")

lamarPatterson = fetch_shots_by_player_id_and_season(203934, "2015-16")
lamarPatterson = data.frame(lamarPatterson$player)
write.csv(lamarPatterson, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/lamarPatterson.csv")

dennisSchroder = fetch_shots_by_player_id_and_season(203471, "2015-16")
dennisSchroder = data.frame(dennisSchroder$player)
write.csv(dennisSchroder, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/dennisSchroder.csv")

mikeMuscala = fetch_shots_by_player_id_and_season(203488, "2015-16")
mikeMuscala = data.frame(mikeMuscala$player)
write.csv(mikeMuscala, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/mikeMuscala.csv")

mikeScott = fetch_shots_by_player_id_and_season(203118, "2015-16")
mikeScott = data.frame(mikeScott$player)
write.csv(mikeScott, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/PistonsVSHawks/mikeScott1.csv")

russell = fetch_shots_by_player_id_and_season(78500, "2015-16")
russell = data.frame(russell$player)
write.csv(russell, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/russell.csv")


test_null = fetch_shots_by_player_id_and_season(1, "2015-16")
test_null = data.frame(test_null$player)
write.csv(mikeScott, file = "E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/Pistons/mikeScott.csv")

allPlayerId = read.csv("E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/allPlayerID.csv")

allPlayerIdNew = allPlayerId[0,]

for (len in 1:nrow(allPlayerId)){
  if (allPlayerId[len,6] <= 2015 && allPlayerId[len,7] >= 2016){
    allPlayerIdNew <- rbind(allPlayerIdNew, allPlayerId[len,])
  }
}

allPlayerIdList = allPlayerIdNew$PERSON_ID
for (id in allPlayerIdList){
  tryCatch({
    test_null = fetch_shots_by_player_id_and_season(id, "2015-16")
    test_null = data.frame(test_null$player)
    write.csv(test_null, file = sprintf('E:/Baiduyun Download/chuichan/17 Spring/Scala/finalProject/allShotsData/.%d.csv',id))
  },error = function(e){})
}

