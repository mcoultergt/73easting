;;INTA 4742/6742 CX4232 CSE6742
;;
;;Spring 2016
;;====================NOTES====================
;; =========Instructor Comments=========
;; =========Research Question=========
;; Broadly - impact of technology
;; narrowly - invest in new technology/sights/etc
;; ==================END NOTES==================


globals [sand M1A1turret_stab driftdegree M1A1gps T72thermal_sights m1a1hitrate t72hitrate T72thermal_sights_range scale_factor_x scale_factor_y t72_shot m1a1_shot targetrange target_direction m1a1_move_speed m1a1_shot_speed desert ridgeline_x_meter t72target m1a1target m1a1p_kill m1a1_armor p_k_t72 p_detection p_detectioniraqi t72targets]  ;; Assume sand is flat after a point...
breed [m1a1s m1a1] ;; US Army M1A1
breed [t72s t72] ;; Iraqi Republican Guard T-72

m1a1s-own [hp fired time_since_shot shot_at crest]       ;; both t72s and m1a1s have hit points
t72s-own [hp fired time_since_shot shot_at]    ;; both t72s and m1a1s have hit points

to setup
  clear-all
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
  set m1a1-main-gun 1
  set lead_t72_x_cor 20
  set lead_t72_y_cor 0
  set extra-t72s true
  set extra_lead_t72_y_cor -8
  set extra_lead_t72_x_cor 22
  set coil-t72s true
  set coil_middle_t72_x_cor 35
  set coil_middle_t72_y_cor 10
  set M1A1_Thermal_Sights 1
  ;set M1A1_Thermal_Sights_Range 2000
  set M1A1_Turret_Stablization true
  set M1A1_GPS true
  set m1a1-formation "Line"
  set m1a1-spacing 10
  set t72-formation "Line"
  set t72-spacing 10
  set Desert_Length_In_Meters 10000
  set Desert_Height_In_Meters 10000
  set ridgeline_x_cor 0
  set desert-visibility 50
  end



to setup-m1a1s
  set-default-shape m1a1s "m1a1" ;; make m1a1s their own shape
  let m1a1-normalized-spacing_x ((m1a1-spacing / 100) * ( max-pxcor)) / initial-number-m1a1 ;;normalize our m1a1 spacing...
  let m1a1-normalized-spacing_y ((m1a1-spacing / 100) * ( max-pycor)) / initial-number-m1a1 ;;normalize our m1a1 spacing...
  let current-m1a1s initial-number-m1a1 ;;initialize counter
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
  ;;if we have an even number of M1A1s we need to make the line accordingly.
  ;let initial-number-m1a1-mod initial-number-m1a1 - 1
  ;if initial-number-m1a1 mod 2 = 0 [ask m1a1 initial-number-m1a1-mod [die] ] ;; mod 2
  ;;create the LEAD m1a1
  ;create-m1a1s 1 [set color sky set size 5 setxy lead_m1a1_x_cor lead_m1a1_y_cor set heading 90 set hp 1]
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
      ifelse max-pxcor - coil_middle_t72_x_cor < 10
      [fd max-pxcor - coil_middle_t72_x_cor ][fd 10]
      set color red
      set size 5
      set hp 1
      ]
      ;layout-circle t72s 10 ;setxy coil_middle_t72_x_cor coil_middle_t72_y_cor set hp 1 ;;hardcode 17 for right now, we can bring this out later if we need.
  ]
end

to setup-technology
end

to setup-desert
  ;;in this function we're going to setup and normalize the desert
  set scale_factor_x max-pxcor / Desert_Length_In_Meters  ;; this will give us a fraction so we can work with xycor easier
  set scale_factor_y max-pycor / Desert_Height_In_Meters  ;;this will give us a fraction so we can work with xycor easier
  let desert-setup min-pycor ;; dynamically allocate our min-pycor...
  let random_num 0 ;;initialize
  while [(desert-setup + random_num) <= max-pycor] ;;while our index (desert-setup) plus our random numeber (random_num) are within the bounds of the map...
  [
  ask patch ( (ridgeline_x_cor - 2) + random 3) (desert-setup + random_num) [set pcolor ( 36 + random-float 3) ];(max-pycor - desert-setup) [set pcolor 37] ;; set the random patch to be a random color based around the 'sand' color
  set desert-setup desert-setup + 1 ;; increment our index
  set random_num (random 2 + random -2)
  ]
  set ridgeline_x_meter ridgeline_x_cor / scale_factor_x
end

to setup-move
  ;; from open source documentation, the top speed (off road) of a M1A1 is 48km/h.
  ;; and since we know our scale factors, we can get that each M1A1 should move 48e-3 * scale_factor per tick...we'll use scale factor X just to be simple.
  set m1a1_move_speed 48000 / 3600 * scale_factor_x ;; M1A1 speed is 48kmh ==> 48000m/h ==> 48000m/3600s  get our move speed in m/s (will be 13.3m/s)
  show scale_factor_x
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
set driftdegree (movetobattle + (2 - (random-float 4)) * (1 - M1A1_GPS))
end

to go
  ;;sanity check and make sure somehow our tanks didn't all destroy each other
  if not any? t72s  [stop]
  if not any? m1a1s [stop]
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
  ask t72s [if hp <= 0 [ die ]]
  ask m1a1s [if hp <= 0 [ die ]]
  clear-links ;; reset links so we can see missed shots (if we're looking...)
end  

to move
   ;; our M1A1s are going to be moving towards the right
   ;;first we'll do a GPS check...if the M1A1s have GPS they'll stay together and hopefully engage at all around the same time. if they don't have GPS, then they'll wander and who knows when they'll engage.
   ;;ifelse M1A1_GPS = True
   ;;[fd m1a1_move_speed]
   ;;show driftdegree
   set heading driftdegree
   fd m1a1_move_speed ;; this is how we'll end up drifting our tanks...roughly by a sum of +-4 degrees. this is probably a little extreme and we can change it later if need be.
   set fired fired - 1 ;;go ahead and decrement the 'fired' variable
   if pxcor >= ridgeline_x_cor
   [
   set crest 1 ;; set our crest variable if they've gone over the hill
   ]

   end

;;TODO - Comment this code!
to detect
  ;;now we are going to create an code block to see if the gunner will see any enemy targets.
  ;show "entering detect"  
  ifelse crest != 1
  [set t72targets t72s in-radius ( ( 4000 * scale_factor_x ) - ridgeline_x_cor )] ;;find any T-72s in visual range, changed to include ridge...)
  [set t72targets t72s in-radius ( ( 4000 * scale_factor_x ))] ;;find any T-72s in visual range AFTER the ridge...)
  let direction_of_view heading - 45 + random-float 90 ;;
  let tank_x_pos xcor;;asign a variable for x cord of "your" tank
  let tank_y_pos ycor;;assign a variable for y cord of "enemy" tank
  let target_x_pos 0
  let target_y_pos 0
  let delta_x 0
  let delta_y 0
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
       let range (distance myself) / scale_factor_x
       set p_detection (1 / ( 1 + exp (( range / (1154 + (11788.68 * M1A1_Thermal_Sights) ) - (1.75 +(8.259 * M1A1_Thermal_Sights))))))
       set random_detect random-float 1
       if random_detect <= p_detection
       [
         set t72target self
       ]
     ] 
   ]
  end

to t72detect
  let t72max_engagement_range 0
  let localt72thermal 0
    ifelse desert-visibility < 800 
    [set localt72thermal 1
     set t72max_engagement_range 800
    ]
    [set localt72thermal 0
     set t72max_engagement_range desert-visibility ;; if the weather is good the T-72s engage using naked eye   
    ]
  let m1a1targets m1a1s in-radius (t72max_engagement_range * scale_factor_x) ;;find any Abrams Tanks in our max engagement range  
  let direction_of_view heading - 45 + random-float 90 ;;
  ;;show direction_of_view
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
     if direction_of_view - 5 < target_direction and direction_of_view + 5 > target_direction
     [
       let range (distance myself) / scale_factor_x
       set p_detectioniraqi (1 / ( 1 + exp (( range / (1154 - 886.8 * localt72thermal)) - (1.75 + (.5475 * localt72thermal)))))
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
  ;;let m1a1max_engagement_range M1A1thermal_sights_range * scale_factor_x ;; set the farthest away patch the M1A1s can engage...assume our thermal sights are our max range.
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
        set targetrange [distance myself] of t72target / scale_factor_x
        if targetrange < 3500
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
                 ask t72target [set hp hp - m1a1p_kill set label "Killed!"] ;; And destoy the target tank if we're <= that probability for heat round
               ] 
          
          ]
          [set label "Miss!"] ;;else label the M1A1 that fired as having missed.
           set fired 3 ;; reset at the end of 3 move turns (set in the 'move' function) we're going to let our turtle fire again. this should 'slow down' the simulation.
      ]
      ]
    ]
  ]
  ]

end

to t72engage
  ;; now we're going to check to see if our enemy T-72s are within our range (defined by M1A1thermal_sights_range) and if they are, use our m1a1hitrate probability to attempt to him them.
  ;; convert our patches into distance...
  set fired fired - 1 ;;we're adding this line in here because the T72s dont' have a move function...
  if m1a1target != 0
  [
    if m1a1target != nobody
      [
      let targetrangem1a1 [distance myself] of m1a1target / scale_factor_x
      if targetrangem1a1 < 2500
      [
      ;if target xcor >= ridgeline_x_cor
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
