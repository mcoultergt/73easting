;;INTA 4742/6742 CX4232 CSE6742
;;
;;Spring 2016
;;==================NOTES===================
;; =========Instructor Comments=========
;; be careful of too many variables
;; IRG defensive? How would those have had a tangiable effect on the outcome of the battle? ONe thing we want to think about when we're doing more recent battles
;; is as technically gets more advanced, communciations technology has a bigger role in the battles. that's one of the challenges for one of the more modern battles.
;; If we have other data and examples to talk about, this is more data and motivation for what we should model and we should try to compare and contrast what we have.
;; the more
;; =========Class Today=========
;; we're goign to do background research into what battle we're doing
;; how did we get here, what's happenening the world, what are we tryign to accmomplish, going throught he timeline of the battle
;; what occured when, what were the decision points for the battle, identify decision points and opporunties . Look at who was involved and
;; what were the types of soldiers and equipment that were involved. Cristicsms of the battle that happened afterware. This can all go in
;; story board of what happened. POSSIBLE PRESENTATION NEXT WEEK?
;; second element - conception plans for the model - what's goign to be important to actually put in
;; =========Research Question=========
;; Broadly - impact of technology
;; narrowly - invest in new technology/sights/etc
;; ==================END NOTES==================

globals [sand M1A1turret_stab M1A1thermal_sights M1A1thermal_sights_range M1A1gps T72turret_stab T72thermal_sights T72gps m1a1hitrate t72hitrate T72thermal_sights_range scale_factor_x scale_factor_y t72_shot m1a1_shot m1a1hitadjust t72hitadjust m1a1_fired t72_fired t72target m1a1target]  ;; Assume sand is flat after a point...
breed [m1a1s m1a1] ;; US Army M1A1
breed [t72s t72] ;; Iraqi Republican Guard T-72

m1a1s-own [hp fired]       ;; both t72s and m1a1s have hit points
t72s-own [hp fired]       ;; both t72s and m1a1s have hit points

to setup
  clear-all
  ask patches [ set pcolor brown ]
  setup-m1a1s   ;; create the m1a1s, then initialize their variables
  setup-t72s ;; create the t72s, then initialize their variables
  setup-technology
  setup-desert
  reset-ticks
end

to setup-m1a1s
  set-default-shape m1a1s "m1a1" ;; make m1a1s their own shape
  let current-m1a1s 1 ;;initialize counter
  ;;initailize loop and let it: create n number of m1a1s with size 5, color blue, facing EAST and in a line, increment counter
  while [current-m1a1s <= (initial-number-m1a1 / 2)]
  [ create-m1a1s 1 [set color blue set size 5 setxy lead_m1a1_x_cor - 2.5 lead_m1a1_y_cor - ((5 * current-m1a1s)) set heading 90 set hp 1 ]
    create-m1a1s 1 [set color blue set size 5 setxy lead_m1a1_x_cor - 2.5 lead_m1a1_y_cor + ((5 * current-m1a1s)) set heading 90 set hp 1 ]
    set current-m1a1s current-m1a1s + 1
  ]
  ;;if we have an even number of M1A1s we need to make the line accordingly.
  let initial-number-m1a1-mod initial-number-m1a1 - 1
  if initial-number-m1a1 mod 2 = 0 [ask m1a1 initial-number-m1a1-mod [die] ] ;; mod 2
  ;;create the LEAD m1a1
  create-m1a1s 1 [set color sky set size 5 setxy lead_m1a1_x_cor lead_m1a1_y_cor set heading 90 set hp 1]
end


to setup-t72s
  set-default-shape t72s "t72" ;; make t72s their own shape
  let current-t72s 1 ;;initialize counter
  ;;initailize loop and let it: create n number of t72s with size 5, color blue, facing WEST and in a line, increment counter
  while [current-t72s <= (initial-number-t72 / 2)]
  [ create-t72s 1 [set color red set size 5 setxy lead_t72_x_cor + 2.5 lead_t72_y_cor - ((5 * current-t72s)) set heading 270 set hp 1]
    create-t72s 1 [set color red set size 5 setxy lead_t72_x_cor + 2.5 lead_t72_y_cor + ((5 * current-t72s)) set heading 270 set hp 1]
    set current-t72s current-t72s + 1
  ]
  ;;if we have an even number of T72s we need to make the line accordingly.
  let initial-number-t72-mod initial-number-t72 - 1
  if initial-number-t72 mod 2 = 0 [ask t72 initial-number-t72-mod [die] ] ;; mod 2
  ;;create the front T-72
  create-t72s 1 [set color green set size 5 setxy lead_t72_x_cor lead_t72_y_cor set heading 270 set hp 1]
end

to setup-technology
  ;;take the booleans and convert into 0 or 1...
     ifelse M1A1_Turret_Stablization = True
       [set M1A1turret_stab 1]
       [set M1A1turret_stab 0]
      ifelse M1A1_Thermal_Sights = True
       [set M1A1thermal_sights 1 set M1A1thermal_sights_range M1A1_Thermal_Sights_Range] ;;1420 was the engagement ranged afforded to McMaster's M1A1 due to thermal sights from his front line account
       [set M1A1thermal_sights 0 set M1A1thermal_sights_range 50] ;;assume an engagement range of 50m if we don't have thermal sights
      ifelse M1A1_GPS = True
       [set M1A1gps 1]
       [set M1A1gps 0]
      ifelse T72_Turret_Stablization = True
       [set T72turret_stab 1]
       [set T72turret_stab 0]
      ifelse T72_Thermal_Sights = True
       [set T72thermal_sights 1 set T72thermal_sights_range T72_Thermal_Sights_Range] ;;assume the Iraqi version to be 1/2 to 1/3 as good.
       [set T72thermal_sights 0 set T72thermal_sights_range 50] ;;assume this is all you can see in a sandstorm...is this a good estimate?
      ifelse T72_GPS = True
       [set T72gps 1]
       [set T72gps 0]
  ;;we'll use a modified version of the empirical formula used on page 36 with data from page 20 incorporating our
  ;;first iteration hit rate
  ;;set m1a1hitrate (0.64 + ( 0.00443299 * M1A1turret_stab ) + ( 0.01676 * M1A1thermal_sights ) + ( 0.02311 * M1A1gps ))
  ;;set t72hitrate (0.5 + ( 0.00543299 * T72turret_stab ) + (0.00676  * T72thermal_sights ) + (0.01311 * T72gps )) / 2
  ;;second iteration hit rate
  set m1a1hitadjust (1 + ( 0.00443299 * (1 - M1A1turret_stab ) ) + ( 0.01676 * (1 - M1A1thermal_sights ) ) + ( 0.02311 * (1 - M1A1gps )))
  set t72hitrate (0.5 + ( 0.00543299 * T72turret_stab ) + (0.00676  * T72thermal_sights ) + (0.01311 * T72gps )) / 2
  ;;in here we'll setup up our technology variables
  ;; note for all this the point of the model isn't to see if the technology should be IMPROVED at all, it's to see if a
  ;; tangible difference exists for having the technology in the first place.
end

to setup-desert
  ;;in this function we're going to setup and normalize the desert.
  ;;entire battle was fought in the span of ~1500 meters, so if we make our entire area 3000 meters, that should be enough maneuvering room.
  set scale_factor_x max-pxcor / Desert_Length_In_Meters  ;; this will give us a fraction so we can work with xycor easier
  set scale_factor_y max-pycor / Desert_Width_In_Meters  ;;this will give us a fraction so we can work with xycor easier
end

to go
  ;;sanity check and make sure somehow our tanks didn't all destroy each other
  if not any? t72s [ stop ]
  if not any? m1a1s [stop]
  ask m1a1s
  [
    move
    detect
    m1a1engage
    death
  ]
  ask t72s
  [
    ;;based on historical data the Iraqi Republican Guard tanks didn't move during the battle.
    t72engage
    death
    ;;reproduce-t72s
  ]
  ;reset all of our shots fired per tick variables
  set m1a1_fired 0
  set t72_fired 0
  ask m1a1s [set fired 0]
  ask t72s [set fired 0]
  clear-links ;; reset links so we can see missed shots (if we're looking...)
  tick
end

to move
   ;; our M1A1s are going to be moving towards the right
   ;;first we'll do a GPS check...if the M1A1s have GPS they'll stay together and hopefully engage at all around the same time. if they don't have GPS, then they'll wander and who knows when they'll engage.
   ifelse M1A1_GPS = True
   [fd 1]
   [rt (random 4 + random -4) fd 1] ;; this is how we're going to implement a slight drift in GPS
end


to detect
  ;;now we are going to create an code block to see if the gunner will see any enemy targets.
  let m1a1targets t72s in-radius 2500 ;;find any T-72s in visual range
  let direction_of_view heading - 45 + random 90 ;;
  let tank_x_pos xcor;;asign a variable for x cord of "your" tank
  let tank_y_pos ycor;;assign a variable for y cord of "enemy" tank
  let target_x_pos 0
  let target_y_pos 0
  let delta_x 0
  let delta_y 0
  let target_direction 0
  let tau 0
  let p_detection 0
  let random_detect 0
  ask m1a1targets
  [set target_x_pos xcor
   set target_y_pos ycor
   set delta_x target_x_pos - tank_x_pos
   set delta_y target_y_pos - tank_y_pos
   set target_direction atan delta_x delta_y
   write "target direction"
   show target_direction
     if direction_of_view - 9 < target_direction and direction_of_view + 9 > target_direction
     [ write "range"
       show distance turtle 1 / scale_factor_x / 1000
       set tau 6.8 * 8 * distance turtle 1 / 14.85 / 2.93 / 1000 / scale_factor_x
       write "tau ="
       show tau
       set p_detection 1 - exp (-30 / tau)
       write "probability of detection"
       show p_detection
       set random_detect random 1
       if random_detect <= p_detection
       [
        set t72target self
        show t72target
       ]
     ]
  ]
  end




to m1a1engage
  ;; now we're going to check to see if our enemy T-72s are within our range (defined by M1A1thermal_sights_range) and if they are, use our m1a1hitrate probability to attempt to him them.
  ;; convert our patches into distance...
  ;;let m1a1max_engagement_range M1A1thermal_sights_range * scale_factor_x ;; set the farthest away patch the M1A1s can engage

  ;;let m1a1targets t72s in-radius m1a1max_engagement_range ;;find any T-72s in our max engagement range
  ;;set t72target min-one-of t72target [distance myself] ;; engage the closest T72
  let shoot false
  if t72target != nobody [ set shoot true ] ;;if there's somebody in range
  if shoot = true
  [
    if fired = 0
    [
   create-link-to t72target [set color blue] ;;show what units the M1A1s are engaging
   let targetrange [distance myself] of t72target / scale_factor_x
   show targetrange
   ;let targetrange 1500
   let cep (m1a1hitadjust * 36 - 35 * exp (-1 * targetrange / 9000)) ;; adjust our cep
   set m1a1hitrate (1 - exp (-.693147 * 100 / (cep * cep))) ;;adjust our m1a1hitrate
   ;;show m1a1hitrate
   set m1a1_shot random 1 ;;have a randomly distributed uniform [0,1].
   if m1a1_shot <= m1a1hitrate ;;check this random number against our hit probability...
    [ask t72target [set hp hp - 1]]
    set m1a1_fired 1 ; this M1A1 already fired this tick!
    ask m1a1s [set fired 1] ;
    ]
  ]



end

to t72engage
  ;; now we're going to check to see if our enemy T-72s are within our range (defined by M1A1thermal_sights_range) and if they are, use our m1a1hitrate probability to attempt to him them.
  ;; convert our patches into distance...
  let t72max_engagement_range t72thermal_sights_range * scale_factor_x ;; set the farthest away patch the M1A1s can engage
  let t72targets m1a1s in-radius t72max_engagement_range ;;find any T-72s in our max engagement range
  let target min-one-of t72targets [distance myself] ;; engage the closest M1A1
  ;;TO DO - IF and ONLY IF we've already been fired on!
  let shoot false
  if target != nobody [ set shoot true ] ;;if there's somebody in range
  ;;let targetrange distance target * scale_factor_x
  if shoot = true
  [
    if fired = 0
    [
      create-links-to t72targets [set color red]
      let targetrange [distance myself] of target / scale_factor_x
      show targetrange
      ;let targetrange 1500
      let cep (t72hitadjust * 36 - 35 * exp (-1 * targetrange / 9000)) ;; adjust our cep
      set t72hitrate (1 - exp (-.693147 * 100 / (cep * cep))) ;;adjust our m1a1hitrate
      show t72hitrate
      set t72_shot random 1 ;;have a randomly distributed uniform [0,1].
      if t72_shot <= t72hitrate ;;check this random number against our hit probability...
      [ask target [set hp hp - 1]]
      set t72_fired 1 ; this M1A1 already fired this tick!
      ask t72s [set fired 1]
     ]

  ]
end




;;to reproduce-t72s  ;; t72s procedure
;;  if random-float 100 < t72s-reproduce [  ;; throw "dice" to see if you will reproduce
;;   set energy (energy / 2)               ;; divide energy between parent and offspring
;;    hatch 1 [ rt random-float 360 fd 1 ]  ;; hatch an offspring and move it forward 1 step
;;  ]
;;end

;;to catch-m1a1s  ;; t72s procedure
;;  let prey one-of m1a1s-here                    ;; grab a random m1a1s
;;  if prey != nobody                             ;; did we get one?  if so,
;;    [ ask prey [ die ]                          ;; kill it
;;      set energy energy + t72s-gain-from-food ] ;; get energy from eating
;;end

to death  ;; turtle procedure
  ;;when energy dips below zero, die
  if hp <= 0 [ die ]
end

;;to display-labels
;;  ask turtles [ set label "" ]
;;  if show-energy? [
;;    ask t72s [ set label round energy ]
;;  ]
;;end

;;to setup-aggregate
;;  set-current-plot "populations"
;;  clear-plot
;;  ;; call procedure generated by aggregate modeler
;;  system-dynamics-setup
;;  system-dynamics-do-plot
;;end

;;to step-aggregate
;;  ;; each agent tick is DT=1
;;  repeat ( 1 / dt ) [ system-dynamics-go ]
;;end

;;to compare
;;  go
;;  step-aggregate
;; set-current-plot "populations"
;;  system-dynamics-do-plot
;; update-plots
;;  display-labels
;;end


; Copyright 2005 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
601
10
1621
1051
50
50
10.0
1
14
1
1
1
0
0
0
1
-50
50
-50
50
0
0
1
ticks
7.0

TEXTBOX
104
10
254
28
Agent Model
11
0.0
0

BUTTON
26
92
89
125
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
96
95
159
128
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
6
129
195
162
initial-number-m1a1
initial-number-m1a1
0
50
8
1
1
m1a1
HORIZONTAL

SLIDER
4
169
176
202
initial-number-t72
initial-number-t72
0
50
17
1
1
t72
HORIZONTAL

SLIDER
4
216
176
249
lead_m1a1_x_cor
lead_m1a1_x_cor
min-pxcor
max-pxcor
-20
1
1
NIL
HORIZONTAL

SLIDER
4
257
176
290
lead_m1a1_y_cor
lead_m1a1_y_cor
min-pycor
max-pycor
0
1
1
NIL
HORIZONTAL

SLIDER
4
295
176
328
lead_t72_x_cor
lead_t72_x_cor
min-pxcor
max-pxcor
21
1
1
NIL
HORIZONTAL

SLIDER
1
338
173
371
lead_t72_y_cor
lead_t72_y_cor
min-pycor
max-pycor
0
1
1
NIL
HORIZONTAL

SWITCH
2
376
180
409
M1A1_Thermal_Sights
M1A1_Thermal_Sights
0
1
-1000

SWITCH
2
454
207
487
M1A1_Turret_Stablization
M1A1_Turret_Stablization
1
1
-1000

SWITCH
1
491
117
524
M1A1_GPS
M1A1_GPS
1
1
-1000

SWITCH
3
527
170
560
T72_Thermal_Sights
T72_Thermal_Sights
0
1
-1000

SWITCH
3
600
198
633
T72_Turret_Stablization
T72_Turret_Stablization
0
1
-1000

SWITCH
4
637
109
670
T72_GPS
T72_GPS
0
1
-1000

MONITOR
292
132
372
177
NIL
m1a1hitrate
7
1
11

MONITOR
292
180
361
225
NIL
t72hitrate
17
1
11

MONITOR
292
232
387
277
NIL
scale_factor_x
6
1
11

SLIDER
2
415
265
448
M1A1_Thermal_Sights_Range
M1A1_Thermal_Sights_Range
0
2000
1450
1
1
meters
HORIZONTAL

TEXTBOX
293
98
443
126
Computed Values from Simulation
11
0.0
1

SLIDER
4
563
256
596
T72_Thermal_Sights_Range
T72_Thermal_Sights_Range
50
2000
731
1
1
meters
HORIZONTAL

SLIDER
4
675
254
708
Desert_Length_In_Meters
Desert_Length_In_Meters
100
10000
2000
1
1
meters
HORIZONTAL

SLIDER
4
710
248
743
Desert_Width_In_Meters
Desert_Width_In_Meters
100
10000
2000
1
1
meters
HORIZONTAL

MONITOR
293
280
419
325
Current P_hit of T72
t72_shot
7
1
11

MONITOR
293
328
429
373
Current P_hit of M1A1
m1a1_shot
7
1
11

PLOT
292
375
492
525
Number Of Tanks
Time
# Of Tanks
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13345367 true "" "plot count m1a1s"
"pen-1" 1.0 0 -2674135 true "" "plot count t72s"

@#$#@#$#@
## WHAT IS IT?

This is a model of the Battle of 73 Easting during Gulf War I in February 1991.

## HOW TO USE IT
Set the position of the lead US Army M1A1 and the lead Iraqi Republican Guard T-72. By default, these positions are indicative of the historic location of the battle.


## THINGS TO NOTICE


## THINGS TO TRY



## NETLOGO FEATURES


## RELATED MODELS



## CREDITS AND REFERENCES
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

m1a1
true
1
Rectangle -13345367 true false 90 75 210 240
Rectangle -13345367 true false 135 15 165 75

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
Rectangle -1 true true 166 225 195 285
Rectangle -1 true true 62 225 90 285
Rectangle -1 true true 30 75 210 225
Circle -1 true true 135 -30 150
Circle -7500403 true false 180 76 116

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

t72
true
1
Rectangle -2674135 true true 90 75 210 240
Rectangle -2674135 true true 135 15 165 75

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
Rectangle -7500403 true true 195 106 285 150
Rectangle -7500403 true true 195 90 255 105
Polygon -7500403 true true 240 90 217 44 196 90
Polygon -16777216 true false 234 89 218 59 203 89
Rectangle -1 true false 240 93 252 105
Rectangle -16777216 true false 242 96 249 104
Rectangle -16777216 true false 241 125 285 139
Polygon -1 true false 285 125 277 138 269 125
Polygon -1 true false 269 140 262 125 256 140
Rectangle -7500403 true true 45 120 195 195
Rectangle -7500403 true true 45 114 185 120
Rectangle -7500403 true true 165 195 180 270
Rectangle -7500403 true true 60 195 75 270
Polygon -7500403 true true 45 105 15 30 15 75 45 150 60 120

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3
@#$#@#$#@
setup
setup-aggregate
repeat 75 [ go step-aggregate ]
@#$#@#$#@
0.001
    org.nlogo.sdm.gui.AggregateDrawing 1
        org.nlogo.sdm.gui.ReservoirFigure "attributes" "attributes" 1 "FillColor" "Color" 192 192 192 15 105 30 30
@#$#@#$#@
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

m1a1_spawn
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 45 150 240 150

@#$#@#$#@
0
@#$#@#$#@
