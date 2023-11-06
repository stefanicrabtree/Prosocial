;This model examines how different governance stratgies can lead to different outcomes of prosociality



breed [lineages lineage] ;not used in this instantiation, but kept in case of future modification


globals [
]


turtles-own [

  pennies          ; resources that turtles have to contribute to public good game
  head
  sanctioned
  cooperate
  my_lineage
  strategy

]


to go

  if ticks >= 500 [ stop ]
  reset-sanctions
  remove-turtles
  play-goods-game
  sanction-new
  divide_goods

  tick
end


to setup
  clear-all
  let i 1
  let k i
  let j ""
  while [i <= num-lineages] [
    set k remainder i 4
    set j item k (list "mm_only" "Despot" "influencer" "copy_defector")
    create-lineages 1 [create-households i j]
    set i i + 1
  ]
  ask patches [set pcolor black ]
  reset-ticks
end


to create-households [name strat]
  ; create the head of the family
  set size 2
  set shape "person"
  set label (word "HoH" name)
  set my_lineage name      ;inherited by whole lineage
  set head who
  setxy random-xcor random-ycor
  set cooperate true
  set sanctioned false
  if global_strategy = "lineage_based" [set strategy strat]
  set pennies 1
  set color blue

  let pop_lineage ceiling (pop_size / num-lineages)
  let N (percent_cooperators * pop_lineage)
  hatch N [
    set size 1
    move-to one-of patches with [ not any? turtles-here ]
    set pennies 1
    set label "C"
    set color green
  ]

  let J (percent_monitors * pop_lineage)
  hatch J [
    set size 1
    move-to one-of patches with [ not any? turtles-here ]
    set pennies 1
    set label "M"
    set color orange
  ]

  let Q (percent_always_defect * pop_lineage)
  hatch Q [
    set size 1
    move-to one-of patches with [ not any? turtles-here ]
    set cooperate false
    set pennies 1
    set label "D"
    set color red
  ]

  let H ((pop_lineage + 1) - (Q + J + N)) ;reluctants
  hatch H [
    set size 1
    move-to one-of patches with [ not any? turtles-here ]
    set cooperate false
    set pennies 1
    set label "R"
    set color blue
  ]


end

to play-goods-game

  ;if leadership, then reluctants follow
  ;if bad leadership, then reluctants have a bad probability of following
  ;cooperators give their pennies to the head


  ask turtles with [label = "C" or label = "infl"] [
    set pennies pennies - cost-of-pg-game
    set cooperate true
    ask turtle head [set pennies pennies + cost-of-pg-game] ;so every turtle gives their fee to their head
  ]

  ask turtles with [label = "R" ] [
    if sanctioned = true [
      set pennies pennies - cost-of-pg-game
      ;; we need the sanction to make them play the game correctly, otherwise they free ride
      ask turtle head [set pennies pennies + cost-of-pg-game] ]
  ]

  ask turtles with [label = "M" ] [
    set pennies pennies - cost-of-pg-game
    set cooperate true
    ask turtle head [set pennies pennies + cost-of-pg-game]
  ]

end


to divide_goods

  ;heads divide the pennies that have been collected from the p.g. game evenly
  ask turtles with [head = who] [
    set pennies pennies * public_goods_game_multiplier
    let my_turtles turtles with [breed = [breed] of myself]
    let share (pennies / count my_turtles)
    let head_decrease (share * count my_turtles)

    ask my_turtles [ ;this includes HoH
      set pennies pennies + share
    ]

    set pennies pennies - head_decrease
  ]

end

to remove-turtles
  ask turtles with [head != who] [
    if pennies < 0
    [
      replace_agents
      die
    ]
   ; [die]
  ]
end



to sanction-new

  if ticks > sanction_time [ ;;this was set to 100 previously
    ask turtles with [label = "M" ] [
      if random-float 1 < prob-sanction
      [
        let my_sanction one-of turtles with [(label = "R" and cooperate = false) or label = "D"]
        if my_sanction != nobody [
          ask my_sanction [
            ifelse sanction_type = "sanction_fine" [
              let my_fine 0

              if pennies < 10 [
                set my_fine (sanction_fine * 0.25 )
                ;set my_fine (pennies * (sanction_fine_percent * .25)
                set pennies pennies - my_fine
              ]

              if pennies >= 10 and pennies < 20 [
                set my_fine (sanction_fine * 0.5)
                set pennies pennies - my_fine ]

              if pennies >= 20 and pennies < 40 [
                set my_fine (sanction_fine * 0.75)
                set pennies pennies - my_fine ]

              if pennies >= 40 and pennies < 60 [
                set my_fine (sanction_fine)
                set pennies pennies - my_fine ]

              if pennies >= 60 [
                set my_fine (sanction_fine * 1.25)
                set pennies pennies - my_fine ]

              ask turtle head [set pennies pennies + my_fine]
            ][

              let mycost (pennies * sanction_fine_percent)
              set pennies pennies - mycost
              ask turtle head [set pennies pennies + mycost]
            ]

            set sanctioned true
            if label = "R" [set cooperate true] ;this should excempt label = "D"

          ]
        ]

        set pennies pennies - sanction_tax
      ]
    ]
  ]

end



to reset-sanctions
  if global_strategy = "mm" [
    if any? turtles with [label = "R" and sanctioned = true ] [
      ask turtles with [label = "R"  and sanctioned = true ] [
        if random-float 1 < .1 [ set sanctioned false set cooperate false ] ; so under no leadership model a random 10% of sanctioned turtles flip back to reluctant non-cooperators
      ]
    ]
  ]

  if global_strategy = "copy_defector" [
    if any? turtles with [label = "R" and sanctioned = true ] [
      ask turtles with [label = "R"  and sanctioned = true ] [
        if random-float 1 < .1 [ assess_fitness ] ; with copy_defector 10% of sanctioned turtles check in with 10 random turtles and make an assessment
      ]
    ]
  ]

  if global_strategy = "despot" [
  ]


  if global_strategy = "influencer" [
    if any? turtles with [label = "R" and sanctioned = true ] [ ;just added
      ask turtles with [label = "R"  and sanctioned = true ] [ ;just added
        if random-float 1 < .1 [ set sanctioned false set cooperate false ] ; so under no leadership model a random 10% of sanctioned turtles flip back to reluctant non-cooperators
      ]
    ]
    influence
  ]
  ;  ;; if there's a local leader, don't change, otherwise, you can revert
end

to influence
  if any? turtles with [cooperate = false ] and any? turtles with [cooperate = true] [
  let coop_pennies mean [pennies] of turtles with [cooperate = true]
  let defect_pennies mean [pennies] of turtles with [cooperate = false]

  if coop_pennies >= defect_pennies [
    ;;choose a random agent, ask their radius of agents to assess global fitness, and to conform

;      if any? turtles with [label = "R" and sanctioned = false and cooperate = false and any? turtles in-radius local_sphereinfluence with [label = "infl"] = false] [
;        ask one-of turtles with [label = "R" and sanctioned = false and cooperate = false and any? turtles in-radius local_sphereinfluence with [label = "infl"] = false] [
;          set label "infl" set cooperate true ];set influencer_done true type "3"]
;      ]
      if any? turtles with [label = "R" and any? turtles in-radius local_sphereinfluence with [label = "infl"] = false] [
        ask one-of turtles with [label = "R" and any? turtles in-radius local_sphereinfluence with [label = "infl"] = false] [
          set label "infl" set cooperate true set sanctioned false]]
      ]

  ]
  ask turtles with [label = "infl"] [

    if any? turtles in-radius local_sphereinfluence with [label = "R" and cooperate = false] [
      ask turtles in-radius local_sphereinfluence with [label = "R" and cooperate = false] [
        if random-float 1 < local_probinfluence [
          set cooperate TRUE    ;influencer convinces them
          set sanctioned false ]
  ] ] ] ;]

end


to assess_fitness

  let top-neighbor max-one-of n-of 10 other turtles [pennies]   ;best of a random ten others
  if [pennies] of top-neighbor <= pennies ; of myself is implicit because they're the ones asking, but switch the equivalence symbol if I got that backwards
  [
    ; do nothing
  ]

  if top-neighbor != nobody and [pennies] of top-neighbor > pennies [
    if [cooperate] of top-neighbor = false
    [
      print "jerks rule"
      set cooperate false
      set sanctioned false
    ]
  ]

end

to stop_game

  if ticks > 500
      [stop]

end

to replace_agents


    let coop_pennies sum [pennies] of turtles with [cooperate = true]
    let defect_pennies sum [pennies] of turtles with [cooperate = false]
    if coop_pennies > defect_pennies
    [
    hatch 1 [
      set size 1
      move-to one-of patches with [ not any? turtles-here ]
      set pennies 1
      set label "C"
      set color green
      ;print "it's a baby C"
    ]
    ]

    if defect_pennies >= coop_pennies
    [
    hatch 1 [
      set size 1
      move-to one-of patches with [ not any? turtles-here ]
      set pennies 1
      set label "R"
      set color blue
      ;print "it's a baby R"
    ]
    ]
;  ]


end
@#$#@#$#@
GRAPHICS-WINDOW
526
10
1034
519
-1
-1
20.0
1
10
1
1
1
0
0
0
1
0
24
0
24
0
0
1
ticks
30.0

BUTTON
4
10
70
43
NIL
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

SLIDER
0
123
196
156
public_goods_game_multiplier
public_goods_game_multiplier
1
5
2.0
0.5
1
NIL
HORIZONTAL

SLIDER
0
235
172
268
cost-of-pg-game
cost-of-pg-game
0
1
1.0
0.25
1
NIL
HORIZONTAL

SLIDER
4
48
176
81
sanction_fine
sanction_fine
0
15
6.0
0.25
1
NIL
HORIZONTAL

SLIDER
3
86
175
119
sanction_tax
sanction_tax
0
5
0.5
0.05
1
NIL
HORIZONTAL

BUTTON
76
11
139
44
NIL
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

PLOT
185
265
525
495
number_pennies
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Monitor" 1.0 0 -955883 true "" "if any? turtles with [label = \"M\"] [plotxy ticks mean [pennies] of turtles with [label = \"M\"]]"
"Defect" 1.0 0 -8053223 true "" "if any? turtles with [label = \"D\"] [plotxy ticks mean [pennies] of turtles with [label = \"D\"]]"
"Coop" 1.0 0 -12087248 true "" "if any? turtles with [label = \"C\"] [plotxy ticks mean [pennies] of turtles with [label = \"C\"]]"
"Reluct-Coop" 1.0 0 -6565750 true "" "if any? turtles with [label = \"R\" and cooperate = true] [\n  plotxy ticks mean [pennies] of turtles with [label = \"R\" and cooperate = true]\n]"
"Reluct-Defect" 1.0 0 -1069655 true "" "if any? turtles with [label = \"R\" and cooperate = false] [\n  plot mean [pennies] of turtles with [label = \"R\" and cooperate = false]\n]"
"Local-leaders" 1.0 0 -13791810 true "" "if any? turtles with [label = \"infl\"] [ plot mean [pennies] of turtles with [label = \"infl\"]]"

BUTTON
146
11
209
44
tick
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
24
157
196
190
prob-sanction
prob-sanction
0
1
0.9
0.1
1
NIL
HORIZONTAL

PLOT
1042
160
1435
518
Agent counts
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Reluct-Defector" 1.0 0 -2139308 true "" "plot count (turtles with [label = \"R\" and cooperate = false]) + 0.2"
"Cooperators" 1.0 0 -12087248 true "" "plot count (turtles with [label = \"C\"])"
"Monitor" 1.0 0 -817084 true "" "plot count (turtles with [label = \"M\"]) + 0.3"
"Local-Leaders" 1.0 0 -13791810 true "" "plot count (turtles with [label = \"infl\"]) + 0.4"
"Defectors" 1.0 0 -5298144 true "" "plot count (turtles with [label = \"D\"]) + 0.1"
"Reluct-Coop" 1.0 0 -8732573 true "" "plot count (turtles with [label = \"R\" and cooperate = true]) + 0.5"

PLOT
1102
10
1302
160
Total pennies
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [pennies] of turtles"

SLIDER
241
89
413
122
local_sphereinfluence
local_sphereinfluence
0
10
10.0
1
1
NIL
HORIZONTAL

SLIDER
241
121
413
154
local_probinfluence
local_probinfluence
0
1
0.9
0.1
1
NIL
HORIZONTAL

SLIDER
25
190
197
223
sanction_time
sanction_time
0
100
50.0
1
1
NIL
HORIZONTAL

MONITOR
435
60
517
105
local-leaders
count turtles with [label = \"infl\"]
0
1
11

SLIDER
240
165
412
198
num-lineages
num-lineages
1
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
0
285
170
318
pop_size
pop_size
10
500
500.0
10
1
NIL
HORIZONTAL

SLIDER
0
390
170
423
percent_cooperators
percent_cooperators
0
0.95
0.45
0.05
1
NIL
HORIZONTAL

SLIDER
0
320
170
353
percent_monitors
percent_monitors
0
0.15
0.07
0.01
1
NIL
HORIZONTAL

SLIDER
0
355
170
388
percent_always_defect
percent_always_defect
0
0.15
0.07
0.01
1
NIL
HORIZONTAL

MONITOR
435
115
517
160
%_reluctant
1 - (percent_monitors + percent_always_defect + percent_cooperators)
2
1
11

CHOOSER
242
12
381
57
global_strategy
global_strategy
"mm" "despot" "influencer" "copy_defector" "lineage_based"
2

SLIDER
235
55
422
88
sanction_fine_percent
sanction_fine_percent
0.1
1
0.1
.1
1
NIL
HORIZONTAL

CHOOSER
240
215
422
260
sanction_type
sanction_type
"sanction_fine" "sanction_fine_percent"
0

@#$#@#$#@
## WHAT IS IT?

The purpose of the model is to explore the effects of different leadership models on the success of cooperative individuals within a general population of cooperators and defectors. The agents participate in a common-pool resource game (CPRG) by spending units of their wealth, called ``pennies'', but may be sanctioned in different ways if they try to collect from the public pool without first contributing. We monitor the wealth and population size of cooperators vs. defectors as the CPRG continues over time to see which leadership strategy results in the highest levels of prosocial behavior.

## HOW IT WORKS

Governance models do not change the way sanctions are imposed, following Hooper et al. those are imposed by the monitors. Instead the governance model influences how the reluctant individuals in the population choose to begin defecting from the CPRG again after being caught defecting by a monitor and forced to cooperate.


In Mutual Monitoring, a randomized 10% of reluctants revert to defection. 
In Despot, a leader "sets the tone" and agents who have defected and been punished do not choose to defect again; they become cooperators.
In Copy Wealthy Defectors, a random 10% of reluctants continue to defect despite having been sanctioned (i.e. "copy-the-successful").
In Influencer, when the mean wealth of cooperative agents is higher a reluctant will decide to become a local "influencer" for cooperation and will convert some of the other reluctants within the local sphere of influence to cooperate.  


The common-pool resource game relies on enough cooperating agents to put money into the pool. Free-riding defectors reduce the value of the common pool such that even the multiplier benefit cannot keep up and the system will crash in a tragedy of the commons. While monitors can reduce the payoff for defectors, they also pay a cost themselves, if there are too many defectors relative to monitors, monitors cannot survive and defectors will drive the system down again. Thus, the rate and manner in which relucants are able to revert to defection has a large impact on the stability of the CPRG economy.  

## HOW TO USE IT

The "global strategy" chooser allows you to choose different governance strategies. This is the most important to look at how different strategies can lead to different outcomes.

Sanction fines are adjusted based on the amount of wealth each agent has. However, play with how the fines impact agent population. Similarly, see how the "public-goods game multiplier" (common pool resource multiplier) can change the outcome. 

You can vary the number of agents, and see how that impacts things.

num-lineages is not active in this model.

## THINGS TO NOTICE

Fines, common-pool resource multipliers, and costs for monitoring all have interesting impacts on the population. Like a Lotka-Volterra model, try to see what kinds of equilibria you can find in the different governance scenarios.

## THINGS TO TRY

Try seeing if you can get cooperators to be the most dominant strategy in a mutual monitoring scenario.

## EXTENDING THE MODEL

What would happen if there was sexual reproduction in this model?


## CREDITS AND REFERENCES

Paul Hooper et al.'s "A Theory of Leadership in Cooperative Groups" from 2010's Journal of Theoretical Biology directly influenced this work.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Sept_19_mm_only_PGG_2" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>count turtles with [label = "M"]</metric>
    <metric>count turtles with [label = "D"]</metric>
    <metric>count turtles with [label = "C"]</metric>
    <metric>count turtles with [label = "R" and cooperate = true]</metric>
    <metric>count turtles with [label = "R" and cooperate = false]</metric>
    <metric>mean [pennies] of turtles with [label = "M"]</metric>
    <metric>mean [pennies] of turtles with [label = "D"]</metric>
    <metric>mean [pennies] of turtles with [label = "C"]</metric>
    <metric>mean [pennies] of turtles with [label = "R" and cooperate = true]</metric>
    <metric>mean [pennies] of turtles with [label = "R" and cooperate = false]</metric>
    <enumeratedValueSet variable="cost-of-pg-game">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_time">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="public_goods_game_multiplier">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_fine">
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-sanction">
      <value value="0.9"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sept_18_copy_defector" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>count turtles with [label = "M"]</metric>
    <metric>count turtles with [label = "D"]</metric>
    <metric>count turtles with [label = "C"]</metric>
    <metric>count turtles with [label = "R" and cooperate = true]</metric>
    <metric>count turtles with [label = "R" and cooperate = false]</metric>
    <metric>mean [pennies] of turtles with [label = "M"]</metric>
    <metric>mean [pennies] of turtles with [label = "D"]</metric>
    <metric>mean [pennies] of turtles with [label = "C"]</metric>
    <metric>mean [pennies] of turtles with [label = "R" and cooperate = true]</metric>
    <metric>mean [pennies] of turtles with [label = "R" and cooperate = false]</metric>
    <enumeratedValueSet variable="cost-of-pg-game">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_time">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="public_goods_game_multiplier">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_fine">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-sanction">
      <value value="0.9"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Nov_6_influencer" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>count turtles with [label = "M"]</metric>
    <metric>count turtles with [label = "D"]</metric>
    <metric>count turtles with [label = "C"]</metric>
    <metric>count turtles with [label = "R" and cooperate = true]</metric>
    <metric>count turtles with [label = "R" and cooperate = false]</metric>
    <metric>count turtles with [label = "infl"]</metric>
    <metric>mean [pennies] of turtles with [label = "M"]</metric>
    <metric>mean [pennies] of turtles with [label = "D"]</metric>
    <metric>mean [pennies] of turtles with [label = "C"]</metric>
    <metric>mean [pennies] of turtles with [label = "R" and cooperate = true]</metric>
    <metric>mean [pennies] of turtles with [label = "R" and cooperate = false]</metric>
    <metric>mean [pennies] of turtles with [label = "infl"]</metric>
    <enumeratedValueSet variable="cost-of-pg-game">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local_sphereinfluence">
      <value value="3"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local_probinfluence">
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_time">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="public_goods_game_multiplier">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_fine">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-sanction">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global_strategy">
      <value value="&quot;influencer&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sept_19_despot" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>count turtles with [label = "M"]</metric>
    <metric>count turtles with [label = "D"]</metric>
    <metric>count turtles with [label = "C"]</metric>
    <metric>count turtles with [label = "R" and cooperate = true]</metric>
    <metric>count turtles with [label = "R" and cooperate = false]</metric>
    <metric>mean [pennies] of turtles with [label = "M"]</metric>
    <metric>mean [pennies] of turtles with [label = "D"]</metric>
    <metric>mean [pennies] of turtles with [label = "C"]</metric>
    <metric>mean [pennies] of turtles with [label = "R" and cooperate = true]</metric>
    <metric>mean [pennies] of turtles with [label = "R" and cooperate = false]</metric>
    <enumeratedValueSet variable="cost-of-pg-game">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_time">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="public_goods_game_multiplier">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_fine">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-sanction">
      <value value="0.9"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="July_7_mm_only_PGG_1_5_Prob_4" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>count turtles with [label = "M"]</metric>
    <metric>count turtles with [label = "D"]</metric>
    <metric>count turtles with [label = "C"]</metric>
    <metric>count turtles with [label = "R" and cooperate = true]</metric>
    <metric>count turtles with [label = "R" and cooperate = false]</metric>
    <metric>mean [pennies] of turtles with [label = "M"]</metric>
    <metric>mean [pennies] of turtles with [label = "D"]</metric>
    <metric>mean [pennies] of turtles with [label = "C"]</metric>
    <metric>mean [pennies] of turtles with [label = "R" and cooperate = true]</metric>
    <metric>mean [pennies] of turtles with [label = "R" and cooperate = false]</metric>
    <enumeratedValueSet variable="cost-of-pg-game">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_time">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="public_goods_game_multiplier">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_fine">
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-sanction">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="prob-sanction">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent_always_defect">
      <value value="0.07"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local_sphereinfluence">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop_size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent_monitors">
      <value value="0.07"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-of-pg-game">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent_cooperators">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_type">
      <value value="&quot;sanction_fine_percent&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local_probinfluence">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_time">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_fine_percent">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-lineages">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global_strategy">
      <value value="&quot;copy_defector&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_fine">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="public_goods_game_multiplier">
      <value value="1.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="prob-sanction">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent_always_defect">
      <value value="0.07"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local_sphereinfluence">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop_size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent_monitors">
      <value value="0.07"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-of-pg-game">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent_cooperators">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_type">
      <value value="&quot;sanction_fine&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local_probinfluence">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_time">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_fine_percent">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-lineages">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global_strategy">
      <value value="&quot;influencer&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_fine">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="public_goods_game_multiplier">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="July_7_mm_only_PGG_2_Prob_9" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>count turtles with [label = "M"]</metric>
    <metric>count turtles with [label = "D"]</metric>
    <metric>count turtles with [label = "C"]</metric>
    <metric>count turtles with [label = "R" and cooperate = true]</metric>
    <metric>count turtles with [label = "R" and cooperate = false]</metric>
    <metric>mean [pennies] of turtles with [label = "M"]</metric>
    <metric>mean [pennies] of turtles with [label = "D"]</metric>
    <metric>mean [pennies] of turtles with [label = "C"]</metric>
    <metric>mean [pennies] of turtles with [label = "R" and cooperate = true]</metric>
    <metric>mean [pennies] of turtles with [label = "R" and cooperate = false]</metric>
    <enumeratedValueSet variable="cost-of-pg-game">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_time">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="public_goods_game_multiplier">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction_fine">
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-sanction">
      <value value="0.9"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
