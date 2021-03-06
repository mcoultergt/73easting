;;INTA 4742/6742 CX4232 CSE6742
;;
;;Spring 2016
;;====================NOTES====================
;; =========Instructor Comments=========
;; =========Research Question=========
;; Broadly - impact of technology
;; narrowly - invest in new technology/sights/etc
;; ==================END NOTES==================

extensions [profiler]
globals [sand driftdegree M1A1thermal_sights M1A1thermal_sights_range t72thermal T72turret_stab T72thermal_sights T72gps m1a1hitrate t72hitrate t72max_engagement_range T72thermal_sights_range scale_factor_x scale_factor_y t72_shot m1a1_shot targetrange target_direction m1a1hitadjust t72hitadjust m1a1_move_speed m1a1_shot_speed desert ridgeline_x_meter t72target m1a1target p_k_105 m1a1_armor p_k_t72 p_detection t72targets p_detectioniraqi m1a1p_kill ridgeline_x_cor Desert_Length_In_Meters Desert_Height_In_Meters]  ;; Assume sand is flat after a point...
breed [m1a1s m1a1] ;; US Army M1A1
breed [t72s t72] ;; Iraqi Republican Guard T-72

m1a1s-own [hp fired time_since_shot shot_at shoot_stop crest]       ;; both t72s and m1a1s have hit points
t72s-own [hp fired time_since_shot shot_at]    ;; both t72s and m1a1s have hit points

to setup
  clear-all
  ;file-close
  ;file-delete "results.txt"
  ;file-open "results.txt"
  ask patches [ set pcolor brown ] ;;goahead and set the initial desert color to sand...
  setup-m1a1s   ;; create the m1a1s, then initialize their variables
  setup-t72s ;; create the t72s, then initialize their variables
  setup-technology
  setup-desert
  setup-move ;;call setup move after setup desert because you need the m1a1 speed..
  reset-ticks
end

to reset
  ;;we'll setup the battle of 73 easting as it occured historically in this function
  set initial-number-m1a1 9
  set initial-number-t72 8
  set lead_m1a1_y_cor 0
  set lead_m1a1_x_cor -20
  ;set m1a1-main-gun 1
  set lead_t72_x_cor 20
  set lead_t72_y_cor 0
  set extra-t72s true
  set extra_lead_t72_y_cor -8
  set extra_lead_t72_x_cor 22
  set coil-t72s true
  set coil_middle_t72_x_cor coil_middle_t72_x_cor
  set coil_middle_t72_y_cor coil_middle_t72_y_cor
  set m1a1-formation "Line"
  set m1a1-spacing 10
  set t72-formation "Line"
  set t72-spacing 10
  set desert-visibility 50
  end



to setup-m1a1s
  set-default-shape m1a1s "m1a1" ;; make m1a1s their own shape
  let m1a1-normalized-spacing_x ((m1a1-spacing / 100) * ( max-pxcor)) / initial-number-m1a1 ;;normalize our m1a1 spacing...
  let m1a1-normalized-spacing_y ((m1a1-spacing / 100) * ( max-pycor)) / initial-number-m1a1 ;;normalize our m1a1 spacing...
  let current-m1a1s initial-number-m1a1 ;;initialize counter
  ask m1a1s
     [set shoot_stop 1]
  ;;initailize loop and let it: create n number of m1a1s with size 5, color blue, facing EAST and in a line, increment counter
  while [current-m1a1s >= (1)]
  [
    if m1a1-formation = "Line"
    [
      ifelse current-m1a1s mod 2 = 1 ;;do this so we end up with the number of units we thought we'd end up with.
    [create-m1a1s 1 [set color blue set size 5 setxy lead_m1a1_x_cor (lead_m1a1_y_cor - m1a1-normalized-spacing_y * current-m1a1s) set heading 90 set hp 1]]
    [create-m1a1s 1 [set color blue set size 5 setxy lead_m1a1_x_cor (lead_m1a1_y_cor - current-m1a1s * (-1 * m1a1-normalized-spacing_y)) set heading 90 set hp 1]]
    ]
    if m1a1-formation = "Vee"
    [
      ifelse current-m1a1s mod 2 = 1 ;;do this so we end up with the number of units we thought we'd end up with.
    [create-m1a1s 1 [set color blue set size 5 setxy (lead_m1a1_x_cor + m1a1-normalized-spacing_x * current-m1a1s) (lead_m1a1_y_cor - m1a1-normalized-spacing_y * current-m1a1s) set heading 90 set hp 1]]
    [create-m1a1s 1 [set color blue set size 5 setxy (lead_m1a1_x_cor + -1 * current-m1a1s * (-1 * m1a1-normalized-spacing_x)) (lead_m1a1_y_cor - current-m1a1s * (-1 * m1a1-normalized-spacing_y)) set heading 90 set hp 1]]
    ]
    if m1a1-formation = "Wedge"
    [
      ifelse current-m1a1s mod 2 = 1 ;;do this so we end up with the number of units we thought we'd end up with.
    [create-m1a1s 1 [set color blue set size 5 setxy (lead_m1a1_x_cor + -1 * m1a1-normalized-spacing_x * current-m1a1s) (lead_m1a1_y_cor - m1a1-normalized-spacing_y * current-m1a1s) set heading 90 set hp 1]]
    [create-m1a1s 1 [set color blue set size 5 setxy (lead_m1a1_x_cor + current-m1a1s * (-1 * m1a1-normalized-spacing_x)) (lead_m1a1_y_cor - current-m1a1s * (-1 * m1a1-normalized-spacing_y)) set heading 90 set hp 1]]
    ]
    if m1a1-formation = "Echelon Left"
    [
      ifelse current-m1a1s mod 2 = 1 ;;do this so we end up with the number of units we thought we'd end up with.
    [create-m1a1s 1 [set color blue set size 5 setxy (lead_m1a1_x_cor - m1a1-normalized-spacing_x * current-m1a1s * -1) (lead_m1a1_y_cor - m1a1-normalized-spacing_y * current-m1a1s) set heading 90 set hp 1]]
    [create-m1a1s 1 [set color blue set size 5 setxy (lead_m1a1_x_cor + -1 * current-m1a1s * m1a1-normalized-spacing_x) (lead_m1a1_y_cor - current-m1a1s * (-1 * m1a1-normalized-spacing_y)) set heading 90 set hp 1]]
    ]
    if m1a1-formation = "Echelon Right"
    [
      ifelse current-m1a1s mod 2 = 1 ;;do this so we end up with the number of units we thought we'd end up with.
    [create-m1a1s 1 [set color blue set size 5 setxy (lead_m1a1_x_cor + m1a1-normalized-spacing_x * current-m1a1s) (lead_m1a1_y_cor + m1a1-normalized-spacing_y * current-m1a1s) set heading 90 set hp 1]]
    [create-m1a1s 1 [set color blue set size 5 setxy (lead_m1a1_x_cor + (current-m1a1s * -1 * m1a1-normalized-spacing_x)) (lead_m1a1_y_cor + -1 * current-m1a1s * m1a1-normalized-spacing_y) set heading 90 set hp 1]]
    ]

    set current-m1a1s current-m1a1s - 1
  ]
  set t72targets 0
end

to setup-t72s
  set-default-shape t72s "t72" ;; make t72s their own shape
  ;;for t72 spacing: we'll increase up to 2.5 patches the distance between T-72s.
  let t72-normalized-spacing_x ((t72-spacing / 100) * ( max-pxcor) / initial-number-t72) ;;normalize our T72 spacing...
  let t72-normalized-spacing_y ((t72-spacing / 100) * ( max-pycor) / initial-number-t72) ;;normalize our T72 spacing...
  let current-t72s initial-number-t72 ;;initialize counter
  ;;initailize loop and let it: create n number of t72s with size 5, color blue, facing WEST and in a line, increment counter
  while [current-t72s >= (1)]
  [
    if t72-formation = "Line"
    [
      ifelse current-t72s mod 2 = 1 ;;do this so we end up with the number of units we thought we'd end up with.
    [create-t72s 1 [set color red set size 5 setxy lead_t72_x_cor (lead_t72_y_cor - t72-normalized-spacing_y * current-t72s) set heading 270 set hp 1]]
    [create-t72s 1 [set color red set size 5 setxy lead_t72_x_cor (lead_t72_y_cor - current-t72s * (-1 * t72-normalized-spacing_y)) set heading 270 set hp 1]]
    ]
    if t72-formation = "Vee"
    [
      ifelse current-t72s mod 2 = 1 ;;do this so we end up with the number of units we thought we'd end up with.
    [create-t72s 1 [set color red set size 5 setxy (lead_t72_x_cor + t72-normalized-spacing_x * current-t72s) (lead_t72_y_cor - t72-normalized-spacing_y * current-t72s) set heading 270 set hp 1]]
    [create-t72s 1 [set color red set size 5 setxy (lead_t72_x_cor + -1 * current-t72s * (-1 * t72-normalized-spacing_x)) (lead_t72_y_cor - current-t72s * (-1 * t72-normalized-spacing_y)) set heading 270 set hp 1]]
    ]
    if t72-formation = "Wedge"
    [
      ifelse current-t72s mod 2 = 1 ;;do this so we end up with the number of units we thought we'd end up with.
    [create-t72s 1 [set color red set size 5 setxy (lead_t72_x_cor + -1 * t72-normalized-spacing_x * current-t72s) (lead_t72_y_cor - t72-normalized-spacing_y * current-t72s) set heading 270 set hp 1]]
    [create-t72s 1 [set color red set size 5 setxy (lead_t72_x_cor + current-t72s * (-1 * t72-normalized-spacing_x)) (lead_t72_y_cor - current-t72s * (-1 * t72-normalized-spacing_y)) set heading 270 set hp 1]]
    ]
    if t72-formation = "Echelon Left"
    [
      ifelse current-t72s mod 2 = 1 ;;do this so we end up with the number of units we thought we'd end up with.
    [create-t72s 1 [set color red set size 5 setxy (lead_t72_x_cor - t72-normalized-spacing_x * current-t72s * -1) (lead_t72_y_cor + t72-normalized-spacing_y * -1 * current-t72s) set heading 270 set hp 1]]
    [create-t72s 1 [set color red set size 5 setxy (lead_t72_x_cor + current-t72s * -1 * t72-normalized-spacing_x) (lead_t72_y_cor + current-t72s * t72-normalized-spacing_y) set heading 270 set hp 1]]
    ]
    if t72-formation = "Echelon Right"
    [
      ifelse current-t72s mod 2 = 1 ;;do this so we end up with the number of units we thought we'd end up with.
    [create-t72s 1 [set color red set size 5 setxy (lead_t72_x_cor + (t72-normalized-spacing_x * current-t72s)) (lead_t72_y_cor + t72-normalized-spacing_y * current-t72s) set heading 270 set hp 1]]
    [create-t72s 1 [set color red set size 5 setxy (lead_t72_x_cor + (current-t72s * -1 * t72-normalized-spacing_x)) (lead_t72_y_cor + -1 * current-t72s * t72-normalized-spacing_y) set heading 270 set hp 1]]
    ]
    set current-t72s current-t72s - 1
  ]
  if extra-t72s = true
  [
    let i_extra 13 ;; there were 13 tanks
    while [i_extra >= 1]
    [
      create-t72s 1 [set color red set size 5 setxy (extra_lead_t72_x_cor + (t72-normalized-spacing_x * i_extra)) (extra_lead_t72_y_cor) set heading 315 set hp 1]
      set i_extra i_extra - 1
    ]
  ]
  if coil-t72s = true
  [
  create-ordered-t72s 17 ;; we're going to make our circle of T72s using the same parameters as the other T72s
      [
      setxy coil_middle_t72_x_cor coil_middle_t72_y_cor
      ifelse max-pxcor - coil_middle_t72_x_cor < 3
      [fd max-pxcor - coil_middle_t72_x_cor ][ fd 3 ]
      set color red
      set size 5
      set hp 1
      ]
  ]
end

to setup-technology
end

to setup-move
  ;; from open source documentation, the top speed (off road) of a M1A1 is 48km/h.
  ;; and since we know our scale factors, we can get that each M1A1 should move 48e-3 * scale_factor per tick...we'll use scale factor X just to be simple.
  set m1a1_move_speed 48000 / 3600 * scale_factor_x ;; M1A1 speed is 48kmh ==> 48000m/h ==> 48000m/3600s  get our move speed in m/s (will be 13.3m/s)
end

;to drift
;let inbattle0


to setup-desert
  ;;in this function we're going to setup and normalize the desert
  ;;figure out our sides based on our desert area...
  let Desert_Square_Meters Desert_Square_Kilometers * 1000000
  set Desert_Length_In_Meters ( sqrt (Desert_Square_Meters))
  ;show Desert_Length_In_Meters
  set Desert_Height_In_Meters ( sqrt (Desert_Square_Meters) )
  set scale_factor_x max-pxcor / Desert_Length_In_Meters  ;; this will give us a fraction so we can work with xycor easier
  set scale_factor_y max-pycor / Desert_Height_In_Meters  ;;this will give us a fraction so we can work with xycor easier
  set ridgeline_x_cor ( lead_t72_x_cor ) - ( 1420 * scale_factor_x )
  let desert-setup min-pycor ;; dynamically allocate our min-pycor...
  let random_num 0 ;;initialize
  while [(desert-setup + random_num) <= max-pycor] ;;while our index (desert-setup) plus our random numeber (random_num) are within the bounds of the map...
  [
  ask patch ( ( ridgeline_x_cor - 2) + random 3) (desert-setup + random_num) [set pcolor ( 36 + random-float 3) ];(max-pycor - desert-setup) [set pcolor 37] ;; set the random patch to be a random color based around the 'sand' color
  set desert-setup desert-setup + 1 ;; increment our index
  set random_num (random 2 + random -2)
  ]
  set ridgeline_x_meter ridgeline_x_cor / scale_factor_x
  ;show ridgeline_x_cor

  ;;setup our t72 sights... based on weather...
    ifelse desert-visibility < 800
    [
     set t72thermal 1
     set t72max_engagement_range 800
    ]
    [
     set t72thermal 0
     set t72max_engagement_range desert-visibility ;; if the weather is good the T-72s engage using naked eye
    ]
    set t72max_engagement_range t72max_engagement_range * scale_factor_x
        ;show t72max_engagement_range
end


to drift
let inbattle 0
let movetobattle 0
ask m1a1s
[set movetobattle heading]
ask m1a1s
[if fired > inbattle
  [set inbattle fired
  set movetobattle target_direction
  ]
]
set driftdegree (movetobattle + (2 - (random-float 4)) * (1 - m1a1GPS))
end

;;;>>>>>>> origin/slynch31-patch-1
to go
  ;;sanity check and make sure somehow our tanks didn't all destroy each other
  if not any? t72s
  [
    ;easting_report
    stop
   ]
  if not any? m1a1s
  [
    ;easting_report
    stop
  ]
  drift
  ask m1a1s
  [
    move
    detect
    m1a1engage
  ]
  ask t72s
  [
    ;;based on historical data the Iraqi Republican Guard tanks didn't move during the battle.
    t72detect
    t72engage
  ]
  tick
  ;ask t72s [if hp <= 0 [ die ]]
  ;ask m1a1s [if hp <= 0 [ die ]]
  clear-links ;; reset links so we can see missed shots (if we're looking...)
end

to move
   ;; our M1A1s are going to be moving towards the right
   ;;first we'll do a GPS check...if the M1A1s have GPS they'll stay together and hopefully engage at all around the same time. if they don't have GPS, then they'll wander and who knows when they'll engage.
   ;;ifelse M1A1_GPS = True
   ;;[fd m1a1_move_speed]
   ;;show driftdegree
   if shoot_stop < 1
   [
    set heading driftdegree
    fd m1a1_move_speed ;; this is how we'll end up drifting our tanks...roughly by a sum of +-4 degrees. this is probably a little extreme and we can change it later if need be.
    set fired fired - 1 ;;go ahead and decrement the 'fired' variable
    ;show ridgeline_x_cor
    ;show pxcor
    if pxcor >= ridgeline_x_cor
     [
     set crest 1 ;; set our crest variable if they've gone over the hill
     ]
   ]
   set shoot_stop shoot_stop - 1

end

;;TODO - Comment this code!
to detect
  ;;now we are going to create an code block to see if the gunner will see any enemy targets.
  ;show "entering detect"
  ifelse crest != 1
  [set t72targets t72s in-radius abs ( ( ( 4000 * scale_factor_x ) - ridgeline_x_cor ) ) ] ;;find any T-72s in visual range, changed to include ridge...)
  [set t72targets t72s in-radius ( ( 4000 * scale_factor_x ))] ;;find any T-72s in visual range AFTER the ridge...)
  let direction_of_view heading - 45 + random-float 90 ;;
  let tank_x_pos xcor;;asign a variable for x cord of "your" tank
  let tank_y_pos ycor;;assign a variable for y cord of "enemy" tank
  let target_x_pos 0
  let target_y_pos 0
  let delta_x 0
  let delta_y 0
  set target_direction 0
  let tau 0
  ;let p_detection 0
  let random_detect 0
  ask t72targets
  [
    ;show "asking t72targets"
   set target_x_pos xcor
   set target_y_pos ycor
   set delta_x target_x_pos - tank_x_pos
   set delta_y target_y_pos - tank_y_pos
   set target_direction atan delta_x delta_y
   ;;write "target direction" ;;removed this line as it was slowing down the simulation... too much information!
   ;;show target_direction ;;removed this line as it was slowing down the simulation... too much information!
     if direction_of_view - 9 < target_direction and direction_of_view + 9 > target_direction
     [
       ;show "target direction..."
       let range (distance myself) / scale_factor_x
       ;; TODO
       ;;add in carefully here to suppress error where there's nothing to aim at...
       ;carefully
       ;[set tau 6.8 * 8 * distance turtle 1 / 14.85 / 2.93 / 1000 / scale_factor_x]
       ;[set tau 1] ;;set tau to one to prevent divide by zero errors.
       ;;need to fix this before final commit!
       ;write "tau ="
       ;show tau
       ifelse M1A1_Thermal_sights = 1
       [ set p_detection 0.99
         ;show "set thermal sights"
         ]
       [ set p_detection (1 - (1 / 3 * (1 - US_Training))) * (1 / ( 1 + exp (( range / (1154 + ( 11788.68 * M1A1_Thermal_Sights ) )  - ( 1.75 + (8.259 * M1A1_Thermal_Sights))))))]
       set random_detect random-float 1
       if random_detect <= p_detection
       [
         ;show "setting t72target!"
         set t72target self
         ;show t72target
        ;show t72target
       ]
     ]
   ]
  end

to t72detect
  let m1a1targets m1a1s in-radius (t72max_engagement_range) ;;find any Abrams Tanks in our max engagement range
  let direction_of_view heading - 45 + random-float 90 ;;
  let tank_x_pos xcor;;asign a variable for x cord of "your" tank
  let tank_y_pos ycor;;assign a variable for y cord of "enemy" tank
  let target_x_pos 0
  let target_y_pos 0
  let delta_x 0
  let delta_y 0
  let t72_target_direction 0
  let random_detect 0
  ask m1a1targets
  [
   set target_x_pos xcor
   set target_y_pos ycor
   set delta_x target_x_pos - tank_x_pos
   set delta_y target_y_pos - tank_y_pos
   set t72_target_direction atan delta_x delta_y
     if direction_of_view - 5 < t72_target_direction and direction_of_view + 5 > t72_target_direction
     [
       let range (distance myself) / scale_factor_x
       set p_detectioniraqi (1 / ( 1 + exp (( range / (1154 - 886.8 * t72thermal)) - (1.75 + (.5475 * t72thermal)))))
     ]
     set random_detect random-float 1
     if random_detect <= p_detectioniraqi
       [
       set m1a1target self
       ]
    ]
  end


to m1a1engage
  ;; now we're going to check to see if our enemy T-72s are within our range (defined by M1A1thermal_sights_range) and if they are, use our m1a1hitrate probability to attempt to him them.
  ;; convert our patches into distance...
  ;;check to make sure our tanks can shoot...
  if hp <= 0 [die stop ]
  let m1pkrand 0
  if t72target != 0
  [
  if crest = 1
  [
    ;let shoot false
    if t72target != nobody
    [
      if fired <= 0 ;; add this catch all so our tanks can be ready to fire during this initial engagement (fired will be < 0)
      [
        create-link-to t72target [set color blue] ;;show what units the M1A1s are engaging
        set label "Fire!" ;; label the M1A1 that fired as such
        ;ask t72target [set shot_at TRUE] ;;the target has been engaged so the T-72s can shoot back... if they're in range...
        set targetrange [distance myself] of t72target / scale_factor_x ;; this put it into meters...
        if targetrange < ( 3500 ) ;;since we just put our target range into meters let's check it against our desert visibility...
        [
        set m1a1hitrate (1 / (1 + (exp ((targetrange / (475.2 + (M1A1_fcs * 235.2))) - (3.31 + (0.438 * M1A1_fcs))))))
        set m1a1_shot random-float 1 ;;have a randomly distributed uniform [0,1].
        ;show m1a1_shot ;; print the randomly distributed uniform [0,1].
        ifelse m1a1_shot <= m1a1hitrate ;;check this random number against our hit probability...
          [
              ;;now that we've hit let's compute probability of kill
              set m1a1p_kill ( 1 / (1 + (exp (targetrange / (3397.9 + (m1a1-main-gun * 9545 )) - (1.059 + (8.9403 * m1a1-main-gun))))))
              set m1pkrand random-float 1
              if m1pkrand < m1a1p_kill
               [
                 ask t72target [set hp hp - 1] ;; And destoy the target tank if we're <= that probability for heat round
               ]

          ]
          []
          ;[set label "Miss!"] ;;else label the M1A1 that fired as having missed.
           set fired 3 ;; reset at the end of 3 move turns (set in the 'move' function) we're going to let our turtle fire again. this should 'slow down' the simulation.
           if M1A1turret_stab < 1
            [set shoot_stop 2 ;;;fire on the move
            ]
      ]
      ]
    ]
  ]
]



end

to t72engage
  ;; now we're going to check to see if our enemy T-72s are within our range (defined by M1A1thermal_sights_range) and if they are, use our m1a1hitrate probability to attempt to him them.
  ;; convert our patches into distance...
  if hp <= 0 [die stop ]
  set fired fired - 1 ;;we're adding this line in here because the T72s dont' have a move function...
  if m1a1target != 0
  [
    if m1a1target != nobody
      [
      let targetrangem1a1 [distance myself] of m1a1target / scale_factor_x ;;put this value into meters
      if targetrangem1a1 < 2500 ;;this is kind of the max value the t72 can shoot at...
      [
      let shoot false ;;reset the check
      if m1a1target != nobody [ set shoot true ] ;;if there's somebody in range
      if (shoot = true)
      [
      if [crest] of m1a1target = 1 ;; if the target is over the ridge
      [
        if fired <= 0 ;; add in our time dependence for our T-72s, just based roughly on the M1A1 speed...might be a good idea to change this later.
        [
          create-link-to m1a1target [set color red] ;;create a red link to M1A1s
          set t72hitrate (1 / ( 1 + exp ( (targetrangem1a1 / 643.5) - 2.97)))
          set t72_shot random-float 1 ;;have a randomly distributed uniform [0,1].
          if t72_shot <= t72hitrate ;;check this random number against our hit probability...
          [
            ;; since we've hit let's see if armor is on
            set p_k_t72 1 / (1 + exp( (targetrangem1a1 / (1050 -(554.53 * m1a1-upgraded-armor))) - (2.5237 - (.5280 * m1a1-upgraded-armor))))
            let t72_shot_kill random-float 1 ;;have a randomly distributed uniform [0,1].
            if t72_shot_kill <= p_k_t72
            [
            ask m1a1target [set hp hp - 1]
            ]
          ]
      set fired 3 ;;reset our fired for t72s.
     ]
    ]
    ]
  ]
  ]
  ]
end


;to easting_report
;  ask m1a1s
;  [file-print hp]
;  file-print "Hello World"
;  file-close;"results.txt"
;  export-plot "Number Of Tanks" "plot.csv"
;  set number_of_iterations number_of_iterations - 1
;  ifelse number_of_iterations > 0
;  [
;  setup
;  go
;  ]
;  [
;    stop
;  ]
;end

;to death  ;; turtle procedure
;  ;;when energy dips below zero, die
;  if hp <= 0 [ die ]
;end

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
@#$#@#$#@
GRAPHICS-WINDOW
481
27
1201
768
35
35
10.0
1
14
1
1
1
0
1
1
1
-35
35
-35
35
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
147
40
210
73
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
211
40
274
73
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
13
101
205
134
initial-number-m1a1
initial-number-m1a1
0
200
9
1
1
m1a1
HORIZONTAL

SLIDER
11
141
183
174
initial-number-t72
initial-number-t72
0
200
13
1
1
t72
HORIZONTAL

SLIDER
1037
871
1209
904
lead_m1a1_x_cor
lead_m1a1_x_cor
min-pxcor
max-pxcor
-18
1
1
NIL
HORIZONTAL

SLIDER
394
833
566
866
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
576
831
748
864
lead_t72_x_cor
lead_t72_x_cor
min-pxcor
max-pxcor
4
1
1
NIL
HORIZONTAL

SLIDER
756
828
928
861
lead_t72_y_cor
lead_t72_y_cor
min-pycor
max-pycor
0
1
1
NIL
HORIZONTAL

TEXTBOX
278
59
428
87
Computed Values from Simulation
11
0.0
1

SLIDER
393
872
680
905
Desert_Square_Kilometers
Desert_Square_Kilometers
0
200
25
1
1
km^2
HORIZONTAL

CHOOSER
12
218
143
263
t72-formation
t72-formation
"Line" "Vee" "Wedge" "Echelon Left" "Echelon Right"
0

SLIDER
685
872
857
905
t72-spacing
t72-spacing
0
100
10
1
1
NIL
HORIZONTAL

SLIDER
863
872
1035
905
m1a1-spacing
m1a1-spacing
0
100
10
1
1
NIL
HORIZONTAL

CHOOSER
214
372
352
417
m1a1-formation
m1a1-formation
"Line" "Vee" "Wedge" "Echelon Left" "Echelon Right"
0

SLIDER
11
180
199
213
desert-visibility
desert-visibility
0
4000
400
1
1
meters
HORIZONTAL

SWITCH
934
826
1048
859
extra-t72s
extra-t72s
0
1
-1000

SWITCH
1213
799
1316
832
coil-t72s
coil-t72s
0
1
-1000

SLIDER
219
872
391
905
extra_lead_t72_x_cor
extra_lead_t72_x_cor
min-pxcor
max-pxcor
18
1
1
NIL
HORIZONTAL

SLIDER
1214
764
1386
797
extra_lead_t72_y_cor
extra_lead_t72_y_cor
min-pycor
max-pycor
-5
1
1
NIL
HORIZONTAL

SLIDER
1213
833
1385
866
coil_middle_t72_x_cor
coil_middle_t72_x_cor
min-pxcor
max-pxcor
24
1
1
NIL
HORIZONTAL

SLIDER
1214
871
1386
904
coil_middle_t72_y_cor
coil_middle_t72_y_cor
min-pycor
max-pycor
13
1
1
NIL
HORIZONTAL

BUTTON
-2
40
146
73
Historical Parameters
reset
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
214
226
386
259
M1A1_fcs
M1A1_fcs
0
1
0
0.0001
1
NIL
HORIZONTAL

SLIDER
215
101
387
134
M1A1_Thermal_Sights
M1A1_Thermal_Sights
0
1
0
0.000001
1
NIL
HORIZONTAL

SLIDER
215
186
389
219
m1a1-upgraded-armor
m1a1-upgraded-armor
0
1
0
.000001
1
NIL
HORIZONTAL

SLIDER
215
266
387
299
m1a1gps
m1a1gps
0
1
0
0.000001
1
NIL
HORIZONTAL

SLIDER
215
143
387
176
m1a1-main-gun
m1a1-main-gun
0
1
0
0.000001
1
NIL
HORIZONTAL

SLIDER
215
300
387
333
M1A1Turret_stab
M1A1Turret_stab
0
1
0
0.000001
1
NIL
HORIZONTAL

PLOT
272
461
472
611
# Of Tanks
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
"default" 1.0 0 -2674135 true "" "plot count t72s"
"pen-1" 1.0 0 -13345367 true "" "plot count m1a1s"

MONITOR
1215
261
1305
306
US Tank P_Hit
m1a1hitrate
4
1
11

MONITOR
1215
372
1316
417
Iraqi Tank P_Hit
t72hitrate
4
1
11

MONITOR
1215
309
1331
354
US Tank P_Kill | Hit
m1a1p_kill
4
1
11

MONITOR
1214
421
1340
466
Iraqi Tank P_Kill | Hit
p_k_t72
4
1
11

MONITOR
1214
213
1348
258
US Tanks P_Detection
p_detection
4
1
11

MONITOR
1213
471
1352
516
Iraqi Tank P_Detection
p_detectioniraqi
4
1
11

MONITOR
1213
521
1369
566
 Distance Between Forces
targetrange
4
1
11

SLIDER
214
335
426
368
US_Training
US_Training
0
1
0
.01
1
% Above Standard
HORIZONTAL

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
10
Rectangle -13345367 true true 135 135 180 180
Rectangle -13345367 true true 150 120 165 135

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
10
Rectangle -13345367 true true 135 135 180 180
Rectangle -13345367 true true 150 120 165 135

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
<experiments>
  <experiment name="experiment" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count m1a1s</metric>
    <metric>count t72s</metric>
    <enumeratedValueSet variable="m1a1-spacing">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lead_t72_y_cor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m1a1-formation">
      <value value="&quot;Line&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="m1a1-main-gun" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="lead_m1a1_y_cor">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="M1A1Turret_stab" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="coil-t72s">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lead_t72_x_cor">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Desert_Square_Kilometers">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t72-formation">
      <value value="&quot;Line&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lead_m1a1_x_cor">
      <value value="-20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t72-spacing">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="m1a1gps" first="0" step="0.25" last="1"/>
    <steppedValueSet variable="M1A1_fcs" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="number_of_iterations">
      <value value="763"/>
    </enumeratedValueSet>
    <steppedValueSet variable="m1a1-upgraded-armor" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="desert-visibility">
      <value value="0"/>
      <value value="400"/>
      <value value="800"/>
      <value value="1500"/>
      <value value="3000"/>
      <value value="4000"/>
    </enumeratedValueSet>
    <steppedValueSet variable="M1A1_Thermal_Sights" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="extra-t72s">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="US_Training" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="extra_lead_t72_y_cor">
      <value value="-8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coil_middle_t72_x_cor">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coil_middle_t72_y_cor">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-m1a1">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extra_lead_t72_x_cor">
      <value value="22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-t72">
      <value value="13"/>
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
