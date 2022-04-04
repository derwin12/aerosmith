' Allknowing2012 - Please dont redistribute at this time...
' 2021 - Stay Safe Everyone
' v0.01 Alpha
' My Channel: https://www.youtube.com/channel/UCi9jCs_DmgtHWqLVPf5_7rA
' BallSaver
' highscores
' skillshot and super Skillshotplayer
' special and extraball show the awards
' danger and tilt 

' TESTING
' PuPlayer.playlistplayex 5,"PupVideos","ARS107-Scene-99.mp4",0,1
' pOverVid 13
' Pup5     15


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' MODES 
' Toys - ..seems to be the toy box multiball
' Love -
' Rats - Center Ramp (f), Toys and L/R Card-Orbit - Super Spinner  
' Dude - Center, Elevator - Super Lanes
' Back - Right Orbit (f) - Super Pops   50 hits 275K each
' Sweet  - Jester,Elevator,L (f) /R Lock Balls - Super Scoring
' Walk - Center (f)/Right Ramp (f) - Super Ramps
' Same - Elevator (f) (then both L/R Target banks) - Super Targets
' Last - Center/ Left (f) & Right Orbit (f) - Super Orbits
'
' 1 hit flashes the toy box light, then If you sink it changes solid .. -- Done
'       -- gets harder in later levels - 
' shoot box to light lock (green) then it can be thrown in to box  -  Done
' Shoot any shot to light crank it up, then score it to add 20s & 500K -- Done
' shoot left & right of toybox to light VIP Pass (ball Save)  - done
' can abort 3 ball multiball - 3 second countdown BEFORE you throw the ball -- done
' spell aer-os-mith to get PF multiplier
' 3 dude lights increases bonus multiplier  - done

' After Mode show Mode Total, light up all shots - get a shot to give the award
' Super pops max 30 pops at 275K each
' jester icons on dmd - count down to 0 to light extra ball - award the jester at random at NON lit Shot - Done
' number below coins  is the number of pop bumper hits needed to increase to next level - starts at 25
' number below that is total score from last pop bumper hit - quite small
' number below jester is spins needed to increase spinner value starts at 50?
' number below is total score from last spinner shot made - quite small - starts at 50?

' ramp sounds -- todo
' aerosmith targets -- once lit, next hits should just 1 time blink the lights
' toybox - each hit lights a blinking green light, in the box turns it green,  -- done
' 2nd mb takes multiple hits to get light to blink, If there is eon flashing then light LOCK on the kicker

' get the 2nd elevator light .. show elevator clipo with ELEVATOR LIT, lights the door in front of eleator - done
' verify the elevator stacks -- nope
' BALL 1 LOCKED 0 to 1 clip  -- done

' when mode is done (or perhaps timed out) .. you see Shots are lit animation, Xs on pf are blinking.. 
'				then next shot that is hit it shows 2x for that show animation and the correspnding X on pf is solid
'				getting that shot awards the SUPER award for that mode, shots are OFF during this..
'               choose song is flashing at CIU 
' SAME is a long 50s to start .. when ball save ended..?  -- done
' when you get AEROSMITH .. you see Shots are lit animation, Xs on pf are blinking.. , next shot awrds that X 2x shot value
' dude each ramp is 500k and inmation of a dancer

' Sweet Emotion - Rules -  https://youtu.be/cngJExYCD6U?t=6330
'
' Notes:
'PUPOverlays    you put 32bit pngs in there and when you play them it will set them as current overlay frame.
'PUPFrames      you put 32bit pngs in there and when you play them it will set the background png (good for performance)
'PUPAlphas      you put 32bit pngs in there and when you play them it will set them as current overlay frame with alpha blending (performance, v1.4.5+)
'PuPShapes      you put 24bit bmps files in there and the pixel color (0,0) will be used as a mask to make a see=through shape.  See twister puppack for example.

Option Explicit
Randomize

Const bUsePlungerForSternKey 	= False	' Defaults to Right Magna Button but you can use Plunger button also	
Const kBallSearchTimeout 		= 15000	'  Start ball search after 15 seconds of no activity
Const DMDMode					= 2 	' 0=None, 1 = Flex/Ultra, 2=PUP (make sure you set PuPDMDDriverType below)
Const doCallouts				= False ' Play random callouts during the game
Const bFlipperSkipEnabled 		= True	' Skip scenes
Const FontScale					= 1		' Scales the PupFonts up/down for different sized DMDs 		[Desktop 0.75]
Const FontScaleDmd				= 0.5	' Scales the SlimDMDFonts up/down for different sized DMDs  [Desktop 0.5]
Const osbactive					= 0 	' Orbital Scoreboard: Set to 0 for off, 1 for only player 1 to be sent, 2 for all scores to be sent.	
										' See link to create obs.vbs: https://docs.orbitalpin.com/vpx-user-settings

Dim AutoQA:AutoQa=False                	'Main QA Testing FLAG setting to false will disable all this stuff.

Dim AutoAI:AutoAI=True				:debugWall2.IsDropped=False:DebugWall1.IsDropped=False
'Dim AutoAI:AutoAI=True				:debugWall2.IsDropped=True:debugWall1.IsDropped=True

'Dim AutoAI:AutoAI=False
'AutoQA=True

If AutoAI then TurnOnAI
		
Dim shotval
Dim shotvalRight

Const bHardMode=False			' Hard Same -- Same Old Song and Dance - Aerosmith lights come in after CIU
								'           -- mode is 30s with CIU of another 20s


Const DigitFont="DIGIT LCD"
Const DMDScrFont="Donnie Solid Narrow"  ' "Impact"
Const DMDMainFont="Donnie Solid Narrow"

'**************************
'   UltraDMD USER Config
'**************************
Const UseFullColor = "True" 			'	ULTRA: Enable full Color on UltraDMD "True" / "False"
Const UltraDMDVideos = True				'	ULTRA: Works on my DMDv3 but seems it causes issues on others
'**************************
'   PinUp Player USER Config
'**************************
dim PuPDMDDriverType: PuPDMDDriverType=0   	' 0=LCD DMD, 1=RealDMD (For FULLDMD use the batch scripts in the pup pack)
dim useRealDMDScale : useRealDMDScale=0    	' 0 or 1 for RealDMD scaling.  Choose which one you prefer.
dim useDMDVideos    : useDMDVideos=True	   	' true or false to use DMD splash videos.
Dim pGameName       : pGameName="aerosmith"	' pupvideos foldername, probably set to cGameName in realworld
'***********TABLE VOLUME LEVELS ********* 
' [Value is from 0 to 1 where 1 is full volume. 
' NOTE: you can go past 1 to amplify sounds]

Const VolBGMusic = 0.9  ' Volume for Video Clips   

Const VolDef = 0.8		' Default volume for callouts 
Const VolSfx = 0.6		' Volume for table Sound effects 

' VolumeDial:
' VolumeDial is the actual global volume multiplier for the mechanical sounds.
' Values smaller than 1 will decrease mechanical sounds volume.
' Recommended values should be no greater than 1.
Const VolumeDial = 0.8

Const BallSize = 50    ' 50 is the normal size
Const BallMass = 1.0     ' 1 is normal ball

'********* UltraDMD **************
Dim UltraDMD:UltraDMD=0
Const UltraDMD_VideoMode_Stretch = 0
Const UltraDMD_VideoMode_Top = 1
Const UltraDMD_VideoMode_Middle = 2
Const UltraDMD_VideoMode_Bottom = 3
Const UltraDMD_Animation_FadeIn = 0
Const UltraDMD_Animation_FadeOut = 1
Const UltraDMD_Animation_ZoomIn = 2
Const UltraDMD_Animation_ZoomOut = 3
Const UltraDMD_Animation_ScrollOffLeft = 4
Const UltraDMD_Animation_ScrollOffRight = 5
Const UltraDMD_Animation_ScrollOnLeft = 6
Const UltraDMD_Animation_ScrollOnRight = 7
Const UltraDMD_Animation_ScrollOffUp = 8
Const UltraDMD_Animation_ScrollOffDown = 9
Const UltraDMD_Animation_ScrollOnUp = 10
Const UltraDMD_Animation_ScrollOnDown = 11
Const UltraDMD_Animation_None = 14
Const UltraDMD_deOn = 1500

'********* End UltraDMD **************

Const BlinkIntFast   = 70
Const BlinkIntDef    = 125
Const BlinkIntSlow   = 500
Const BlinkPatternDef  = 10
Const BlinkPatternSlow = 100

Dim luts, lutpos
luts = array("ColorGradeLUT256x16_1to1", "ColorGradeLUT_Bright2", "ColorGradeLUT_Bright", "ColorGradeLUT256x16_ConSat", "ColorGradeLUT256x16_HalfSat" )
lutpos = 0	

' Main Code
Dim bDebugMode:bDebugMode = False		' Magna buttons perform debug functions
' Define any Constants
Const cGameName = "aerosmith"		' Match DOF Config Tool
Const TableName = "aerosmith"
Const myVersion = "1.0.0"
Const MaxPlayers = 4		
Dim MusicDir	

'*****************************************
'      Structure to save/restore all player data before moving to a new ones
' ***************************************
Const kStack_Pri0 = 0		' base mode
Const kStack_Pri1 = 1		' toybox, elevator mb
Const kStack_Pri2 = 2		' wizard
Const kStack_Pri3 = 3		' Skillshot

Class cArrowState
	Public ArrowColor
	Public ArrowState
	Public MultipState
	Public NameState
End Class

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)			
Dim BonusHeldPoints(4)
Dim BonusMultiplier
Dim PlayMultiplier
Dim ModePoints
Dim ModePointsSave

Dim WizardModePoints
Dim WizardBonusPoints
			
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(5)   ' GC, 1,2,3, Med-GC
Dim HighScoreName(5)
Dim TiltCount(4)
Dim Jackpot
Dim VipValue
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue
Dim bCreatedBall	' we just created a ball
Dim bAutoPlunger	' Auto fire 
Dim bAutoPlunged	' Did AutoPlunger Fire
Dim bInstantInfo
Dim BumperMultiplier 
dim bLaneSaverEnabled
Dim FlipperSkipCmd		' Command to when you FlipperSkip
Dim VolMusic
Dim BallSearchCnt
Dim bResetCurrentGame
Dim SongNum
Dim ModeCountdown
Dim modeRampColor
Dim bDebounce
Dim bPauseTimer
Dim Multiplier3x

Dim baLightsSaved
Dim baSaveLightsSaved
Dim baRampLightsSaved

Dim bShotMultiplierSelect

Dim modesStarted
Dim ModesCompleted

Dim bFinalTourDone
Dim bFinalTourReady
Dim bFinalTourCount

Dim bMedleyTourProgress(8)	'
Dim bMedleyTourDone			
Dim bMedleyTourReady		' ready to start - progressed thru the prior modes
Dim bMedleyTourCount		' how many tour modes completed

Dim Coins 
Dim HiddenJesters
Dim Dice
Dim SpinHits
Dim SpinScore
Dim SpinScoreCurrent
Dim SpinValue

Dim SpinnerBonus
Dim TargetBonus
Dim BumperBonus
Dim LoopBonus
Dim RampBonus
Dim LaneBonus

Dim PopHits
Dim PopScore
Dim PopScoreCurrent
Dim PopValue
Dim PopLevel
Dim SwitchHitCount

Dim BonusModeTotal(8)		' Holds the total bonus during gameplay
Dim BonusModeValue(8)		' Holds the bonus value that will get added to the next mode bonus
Dim ModeNames(8)
Dim ModeNamesShort(8)
Dim Shots(8)

Dim ModeProgress(8)
Dim ModeOrder(8)			' retain what order they were attempted in as this is used in the Medely/Final Tour
Dim ModePercent(8)
dim Mode2Percent(8)
dim Mode2Progress(8)
Dim Mode2Value(8)
Dim Mode2Total(8)

Dim bModeProgressUpgraded

Dim ToyBoxMBFinished			' Did we complete ToyBoxMB
Dim ToyBoxMultiBallCount		' How many times have we completed ToyBox MB
Dim ToyBoxMBLocks				' How many ToyBox Locks we have done during this toybox MB. We can only lock it a total of 3 times in MB
Dim ToyBoxMultiBallToggle
Dim ToyBoxMBJackpotHits
Dim ToyBoxMBJackpot
Dim ToyBoxMBJackpotBase
Dim ToyBoxMBJackpotTotal
Dim ToyBoxMBAttempts
Dim bToyBoxBonus3x
Dim EndToyBoxMB
Dim ElevTarget1Toggle
Dim ElevMultiBallCount			' How many times have we completed elevator MB
Dim ElevMBJackpotHits
Dim ElevMBJackpot
Dim ElevMBJackpotTotal
Dim bElevMultiBall

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim RealBallsInLock
Dim SpellAerosmith
Dim MultiplierShot
Dim bShotMultiplier
Dim bSecondMode
Dim bBonusMode
Dim	PlayerMode2
Dim	bWizardMode

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim BallSaverActiveBuffer		' Allows lanes to kick in ball saver 
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMusicOn

dim SuperSkillShotIndex
Dim bSkillshotsReady(2)

Dim bPlayerModeSelect
Dim PlayerMode
Dim SaveMode
Dim bRndModeShot

Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bSkipWaitVideo

Dim plungerIM 'used mostly as an autofire plunger
Dim SmartButtonCount
Dim bTableReady
Dim bUseUltraDMD
Dim bUsePUPDMD
Dim bPupStarted
Dim sndGeneralHit
Dim PlayerState(5)
Dim RampLightsSaveColor()
Dim LightsSaveColor()
Dim saveColor()

Const defBallSaverTime = 8		' In seconds
Dim BallSaverTime
BallSaverTime = defBallSaverTime
Const BallsPerGame = 3   ' 3 or 5
Const MaxMultiballs = 5  ' max number of balls during multiballs
Const MaxMultiplier=9

'***************************************************************************
'      Structure to save/restore all player data before moving to a new ones
' **************************************************************************

Const StackSize = 4		' How Many Modes can be stacked  - Higher priority wins    base, mb, wizard, skillshot

Class cStack
	Public bStackActive					' Is there a mode here actually stacked?
	Public ModeIndex					' What is the index of this mode?
	Public ArrowStates(9)				' What are the arrow states for this mode

	Private Sub Class_Initialize(  )
		dim i
		For i = 0 to 9
			set ArrowStates(i) = New cArrowState
		Next
	End Sub

	Public Sub Reset()
		dim i
		bStackActive = False
		ModeIndex = -1
		For i = 0 to 9
			ArrowStates(i).ArrowColor = ""
			ArrowStates(i).ArrowState = 0
			ArrowStates(i).MultipState = 0 	' 
			ArrowStates(i).NameState = 0 	' Color is always name color
		Next 
	End Sub

	Public Function GetArrowState(light)
		dim lIndex
		lIndex = GetModeIndex(light.name)
		GetArrowState = ArrowStates(lIndex).ArrowState
	End Function 

	Public Function GetArrowColor(light)
		dim lIndex
		lIndex = GetModeIndex(light.name)
		GetArrowColor = ArrowStates(lIndex).ArrowColor
	End Function 

	Public Sub SetArrowState(light, state)
		dim lIndex
		lIndex = GetModeIndex(light.name)
		ArrowStates(lIndex).ArrowState = state
	End Sub 

	Public Sub Enable (NewModeIndex) 
		bStackActive = True
		ModeIndex = NewModeIndex
	End Sub 

	Public Sub Disable() 
		Reset
	End Sub 
End Class 

Public StackState(4)			' Stores which modes are actually in play (stacked)


Class TableState
	' These should hold state of the current mode
	Public sBumperMultiplier
	Public bFirstBall
	Public ArrowColor
	Public lArrowStates(9)
	Public lToyBoxMBState			' 
	Public bExtraball_1				' Triggered - Toy or Elev MB Extra Ball
	Public bExtraball_2				' Triggered 3 Modes are started
	Public bExtraball_3				' Triggered 6 Modes are complete
	Public sPopValue
	Public sPopLevel


	' Holds global info
	Public SModeProgress(9)			'
	Public SModeOrder(9)
	Public SModePercent(9)
	Public SMode2Progress(9)		'
	Public SMode2Percent(9)
	Public SMode2Value(9)

	Public AerosmithCount

	Public bSFinalTourDone
	Public bSFinalTourReady
	Public bSFinalTourCount

'  partial
	Public Sub Reset()
		bFirstBall = True
		AerosmithCount = 0
		PopValue=2000
		SpinHits=25
		SpinValue=200
		PopLevel=0
		PopHits=25
	End Sub

	Public Sub Save()
		sBumperMultiplier = BumperMultiplier
		AerosmithCount = SpellAerosmith
		sPopValue = PopValue
		sPopLevel = PopLevel

		bSFinalTourDone = bFinalTourDone
		bSFinalTourReady= bFinalTourReady
		bSFinalTourCount = bFinalTourCount

' save the uservalue of the LockLights
	End Sub

	Public Sub Restore()
		If PlayersPlayingGame = 1 then exit sub
		BumperMultiplier = sBumperMultiplier
		SpellAerosmith = AerosmithCount
		PopValue = sPopValue
		PopLevel = sPopLevel

		bFinalTourDone = bSFinalTourDone
		bFinalTourReady= bSFinalTourReady
		bFinalTourCount = bSFinalTourCount
	End Sub

End Class

Sub TableState_Init(Index)
	Set PlayerState(Index) = New TableState
	PlayerState(Index).Reset
End Sub
Sub StackState_Init(Index)
	Set StackState(Index) = New cStack
	StackState(Index).Reset
End Sub 

'************************
' nFozzy Flipper Physics
'************************
dim LF:Set LF = New FlipperPolarity
dim RF:Set RF = New FlipperPolarity

LoadCoreFiles
InitPolarity

Dim mHMagnet
Set mHMagnet = New cvpmMagnet : With mHMagnet
		.InitMagnet HMagnet, 100
		.GrabCenter = False	
		.MagnetOn = False
End With

mHMagnet.MagnetOn = True

'====================== Routines
Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox  "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox  "You need the controller.vbs in order to run this table, available in the vp10 package"
    On Error Goto 0

	'============================'  Orbital Scoreboard'============================	
	If osbactive = 1 or osbactive = 2 Then		
		On Error Resume Next		
		ExecuteGlobal GetTextFile("osb.vbs")	
		On Error Goto 0
	End if
End Sub

'===================== Common Routines
Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		x.TimeDelay = 44
	Next

	rf.report "Velocity"
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.2, 	1.07
	addpt "Velocity", 2, 0.41, 1.05
	addpt "Velocity", 3, 0.44, 1
	addpt "Velocity", 4, 0.65, 	1.0'0.982
	addpt "Velocity", 5, 0.702, 0.968
	addpt "Velocity", 6, 0.95,  0.968
	addpt "Velocity", 7, 1.03, 	0.945


	rf.report "Polarity"
	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.16, -4.7	
	AddPt "Polarity", 2, 0.33, -4.7
	AddPt "Polarity", 3, 0.37, -4.7	'4.2
	AddPt "Polarity", 4, 0.41, -4.7
	AddPt "Polarity", 5, 0.45, -4.7 '4.2
	AddPt "Polarity", 6, 0.576,-4.7
	AddPt "Polarity", 7, 0.66, -2.8'-2.1896
	AddPt "Polarity", 8, 0.743, -1.5
	AddPt "Polarity", 9, 0.81, -1.5
	AddPt "Polarity", 10, 0.88, 0

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, If .object is used.

'Called with flipper - 
'ProcessBalls - catches ball data. 
' - OR - 
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.

'******************************************************
'******************************************************
'******************************************************
'******************************************************
'******************************************************
'		FLIPPER CORRECTION SUPPORTING FUNCTIONS
'******************************************************

RightFlipper.timerinterval=1
rightflipper.timerenabled=True

sub RightFlipper_timer()

	If leftflipper.currentangle = leftflipper.endangle and LFPress = 1 then 
		leftflipper.eostorqueangle = EOSAnew
		leftflipper.eostorque = EOSTnew
		LeftFlipper.rampup = EOSRampup
		If LFCount = 0 Then LFCount = GameTime
		If GameTime - LFCount < LiveCatch Then
			leftflipper.Elasticity = 0.1
			If LeftFlipper.endangle <> LFEndAngle Then leftflipper.endangle = LFEndAngle
		Else	
			leftflipper.Elasticity = FElasticity
		end if
	elseIf leftflipper.currentangle > leftflipper.startangle - 0.05  Then
		leftflipper.rampup = SOSRampup
		leftflipper.endangle = LFEndAngle - 3
		leftflipper.Elasticity = FElasticity
		LFCount = 0
	elseIf leftflipper.currentangle > leftflipper.endangle + 0.01 Then 
		leftflipper.eostorque = EOST
		leftflipper.eostorqueangle = EOSA
		LeftFlipper.rampup = Frampup
		leftflipper.Elasticity = FElasticity
	end if

	If rightflipper.currentangle = rightflipper.endangle and RFPress = 1 then
		rightflipper.eostorqueangle = EOSAnew
		rightflipper.eostorque = EOSTnew
		RightFlipper.rampup = EOSRampup
		If RFCount = 0 Then RFCount = GameTime
		If GameTime - RFCount < LiveCatch Then
			rightflipper.Elasticity = 0.1
			If RightFlipper.endangle <> RFEndAngle Then rightflipper.endangle = RFEndAngle
		Else
			rightflipper.Elasticity = FElasticity
		end if
	elseIf rightflipper.currentangle < rightflipper.startangle + 0.05 Then
		rightflipper.rampup = SOSRampup 
		rightflipper.endangle = RFEndAngle + 3
		rightflipper.Elasticity = FElasticity
		RFCount = 0 
	elseIf rightflipper.currentangle < rightflipper.endangle - 0.01 Then 
		rightflipper.eostorque = EOST
		rightflipper.eostorqueangle = EOSA
		RightFlipper.rampup = Frampup
		rightflipper.Elasticity = FElasticity
	end if

end sub

dim LFPress, RFPress, EOST, EOSA, EOSTnew, EOSAnew
dim FStrength, Frampup, FElasticity, EOSRampup, SOSRampup
dim RFEndAngle, LFEndAngle, LFCount, RFCount, LiveCatch

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
FStrength = LeftFlipper.strength
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
EOSTnew = 1.0 'FEOST
EOSAnew = 0.2
EOSRampup = 1.5 
SOSRampup = 8.5 
LiveCatch = 8

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle


Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub


Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart, FlipperEnd, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	
	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub
	
	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : If IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end If : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : If IsObject(aInput) then FlipperEnd = aInput.x else FlipperEnd = aInput : end If : End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property
	
	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			Case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		If gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray) 	'debug, reports all coords in tbPL.text
		If not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			Case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
			Case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub
	
	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : If IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end If : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			If TypeName(balls(x) ) = "IBall" then 
				If aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub
	
	Public Sub Fire() 
		Flipper.RotateToEnd
		processballs
	End Sub

	Public Property Get Pos 'returns % position a ball. For debug stuff.
		dim x : for x = 0 to uBound(balls)
			If not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next		
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			If not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
				If DebugOn then StickL.visible = True : StickL.x = balldata(x).x		'debug TODO
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
		If abs(Flipper.currentAngle - Flipper.EndAngle) < 30 Then
			PartialFlipCoef = 0
		End If
	End Sub
	Private Function FlipperOn() : If gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
			dim teststr : teststr = "Cutoff"
			tmp = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
			If tmp < 0.1 then 'If real ball position is behind flipper, exit Sub to prevent stucks	'Disabled 1.03, I think it's the Mesh that's causing stucks, not this
				If DebugOn then TestStr = "real pos < 0.1 ( " & round(tmp,2) & ")" : tbpl.text = Teststr 
				'RemoveBall aBall
				'Exit Sub
			end if

			'y safety Exit
			If aBall.VelY > -8 then 'ball going down
				If DebugOn then teststr = "y velocity: " & round(aBall.vely, 3) & "exit sub" : tbpl.text = teststr
				RemoveBall aBall
				exit Sub
			end if
			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				If aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					'TB.TEXT = balldata(x).id & " " & BALLDATA(X).X & VBNEWLINE & FLIPPERSTART & " " & FLIPPEREND
					If ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			'Velocity correction
			If not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				If DebugOn then set tmp = new spoofball : tmp.data = aBall : End If
				If IsEmpty(BallData(idx).id) and aBall.VelY < -12 then 'If tip hit with no collected data, do vel correction anyway
					If PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1) > 1.1 then 'adjust plz
						VelCoef = LinearEnvelope(5, VelocityIn, VelocityOut)
						If partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
						If Enabled then aBall.Velx = aBall.Velx*VelCoef'VelCoef
						If Enabled then aBall.Vely = aBall.Vely*VelCoef'VelCoef
						If DebugOn then teststr = "tip protection" & vbnewline & "velcoef: " & round(velcoef,3) & vbnewline & round(PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1),3) & vbnewline
						'debug.print teststr
					end if
				Else
		 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
					If Enabled then aBall.Velx = aBall.Velx*VelCoef
					If Enabled then aBall.Vely = aBall.Vely*VelCoef
				end if
			End If

			'Polarity Correction (optional now)
			If not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity If left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				If Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			'debug
			If DebugOn then
				TestStr = teststr & "%pos:" & round(BallPos,2)
				If IsEmpty(PolarityOut(0) ) then 
					teststr = teststr & vbnewline & "(Polarity Disabled)" & vbnewline
				else 
					teststr = teststr & "+" & round(1 *(AddX*ycoef*PartialFlipcoef),3)
					If BallPos >= PolarityOut(uBound(PolarityOut) ) then teststr = teststr & "(MAX)" & vbnewline else teststr = teststr & vbnewline end if	
					If Ycoef < 1 then teststr = teststr &  "ycoef: " & ycoef & vbnewline
					If PartialFlipcoef < 1 then teststr = teststr & "PartialFlipcoef: " & round(PartialFlipcoef,4) & vbnewline				
				end if

				teststr = teststr & vbnewline & "Vel: " & round(BallSpeed(tmp),2) & " -> " & round(ballspeed(aBall),2) & vbnewline
				teststr = teststr & "%" & round(ballspeed(aBall) / BallSpeed(tmp),2)
				tbpl.text = TestSTR
			end if
		Else
			'If DebugOn then tbpl.text = "td" & timedelay
		End If
		RemoveBall aBall
	End Sub
End Class

'================================
'Helper Functions
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
		If not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 then offset = 0
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
		If IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub


Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

Function NullFunctionZ(aEnabled):End Function	'1 argument null function placeholder	 TODO move me or replac eme

Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class


Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		If xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	If xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'Clamp If on the boundry lines
	'If L=1 and Y < yLvl(LBound(yLvl) ) then Y = yLvl(lBound(yLvl) )
	'If L=uBound(xKeyFrame) and Y > yLvl(uBound(yLvl) ) then Y = yLvl(uBound(yLvl) )
	'clamp 2.0
	If xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	If xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function


' DONT FORGET TO CLEAR FlipperSkipCmd when you are past this stage.  
'		What this means is once you move on to the next command (without skipping) you need to clear this otherwise 
'		when you hit both flippers during gameplay it will call the command you put
Sub SetFlipperSkipCmd(Cmd)
	If FlipperSkipCmd="" then 
		FlipperSkipCmd=Cmd
	End If 
End Sub 


Sub StartInstantInfo(keycode)
	D2 "Start Instant " & keycode & " " & bInstantInfo
	If bInstantInfo = False and tmrSkillshot.Enabled=False  Then ' I am already in instantinfo
		InstantInfoTimer.Interval = 8000
		InstantInfoTimer.Enabled = True
		InstantInfoTimer.UserValue=keycode
	End If 
End Sub 


Sub InstantInfoTimer_Timer
    InstantInfoTimer.Enabled = False
    bInstantInfo = True
	D "InstantInfotimer Expired"
	PuPlayer.LabelShowPage pOverVid, 1,0,""
	pCurAttractPos=0
	pInAttract=true
	pAttractNext
End Sub


Sub EndFlipperStatus(keycode)
    If bInstantInfo Then
		D "EndInstantInfo check" & keycode & " " & InstantInfoTimer.UserValue
		If (keycode=InstantInfoTimer.UserValue) then 	' They let go of the key
			D "EndInstantInfo"
			InstantInfoTimer.Enabled = False
			bInstantInfo=False
			PuPlayer.LabelShowPage pBackglass,1,0,""
			pInAttract=false
			playclear pOverVid
			PuPlayer.LabelSet pOverVid,"OverMessage1"," ",1,""
			PuPlayer.LabelSet pOverVid,"OverMessage2"," ",1,""
			PuPlayer.LabelSet pOverVid,"OverMessage3"," ",1,""
			RefreshPlayerMode
			If bPlayerModeSelect Then
				If modecountdowntimer.enabled Then
					pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1))+1)
				Else
					pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1)))
				End If
			End If 
		Else ' They pressed the other flipper so cycle faster 
			PriorityReset=2000
			pAttractNext
		End If 
	Else
		D2 "Stop Instant " & keycode
		InstantInfoTimer.Enabled = False
    End If
End Sub

'a1a1
Sub UpdatePlayerMode() 'Updates the DMD & lights with the chosen skillshots
	D "UpdatePlayerMode " & PlayerMode 
    LightSeqSkillshot.Play SeqAllOff
    'turn off various mode lights
	BumperLight001.State = 0
	BumperLight002.State = 0
	BumperLight003.State = 0
    I15.State = 0:I15.BlinkInterval=BlinkIntDef:I15.BlinkPattern=BlinkPatternDef  'MODE LIGHTS ON PF
    I16.State = 0:I16.BlinkInterval=BlinkIntDef:I16.BlinkPattern=BlinkPatternDef
    I17.State = 0:I17.BlinkInterval=BlinkIntDef:I17.BlinkPattern=BlinkPatternDef
    I18.State = 0:I18.BlinkInterval=BlinkIntDef:I18.BlinkPattern=BlinkPatternDef
    I19.State = 0:I19.BlinkInterval=BlinkIntDef:I19.BlinkPattern=BlinkPatternDef
    I20.State = 0:I20.BlinkInterval=BlinkIntDef:I20.BlinkPattern=BlinkPatternDef
    I21.State = 0:I21.BlinkInterval=BlinkIntDef:I21.BlinkPattern=BlinkPatternDef

	
	'PuPlayer.playlistplayex pMusic,"audioclear","clear.mp3",100, 1
	SaveMode=PlayerMode
	PlaySong "Song-" & PlayerMode & ".mp3"

	dim time, a
	time = 24
	If bUsePUPDMD then time = -1

	'***********************  Turn off last player *****************

	D "UpdatePlayerMode - turn off shot lights"
	SetLightColor I35, "white", 0	
	SetLightColor I62, "white", 0
	SetLightColor I81, "white", 0
	SetLightColor I86, "white", 0
	SetLightColor I91, "white", 0
	SetLightColor I97, "white", 0
	SetLightColor I103, "white", 0
	SetLightColor I107, "white", 0

	For each a in aerosmithLights
		SetLightColor a, "orange", 0
	Next

    DMDFlush
	Dim Line1
	If bUsePupDMD Then
		Line1 = "USE FLIPPERS TO SELECT SONG"
	Else
		Line1 = ">Select Song<"
	End If
    Select Case PlayerMode
		Case -1:' No Mode is selected yet
			DisplayDMDText2 "SELECT", "A SONG", "", eNone, eNone, eNone, time, False, ""   
        Case 0:  ' last
			DMD Line1, CL(0, ModeNamesShort(PlayerMode)), "", eNone, eNone, eNone, time, False, "":I15.State = 2 ' flash left orbits, solid center ramp & right ramp
			modeRampColor = "cyan"
			SetLightColor  I62, modeRampColor, 2
			SetLightColor I107, modeRampColor, 2
			SetLightColor  I86, modeRampColor, 1
			SetLightColor I103, modeRampColor, 1
			ModeCountdown = 50 ' seconds
			If bHardMode Then ModeCountdown = 30
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_last) 'select image
			RefreshPlayerMode
          Case 1: ' walk
			DMD Line1, CL(0, ModeNamesShort(PlayerMode)), "", eNone, eNone, eNone, time, False, "":I16.State = 2' solid orbits, flash ramps
			modeRampColor = "orange"
			SetLightColor I62, modeRampColor, 1
			SetLightColor I107, modeRampColor, 1
			SetLightColor I86, modeRampColor, 2
			ModeCountdown = 50 ' seconds
			If bHardMode Then ModeCountdown = 30
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_walk)
			RefreshPlayerMode
        Case 2:  ' same
			DMD Line1, CL(0, ModeNamesShort(PlayerMode)), "", eNone, eNone, eNone, time, False, "":I17.State = 2 ' flash elevator
			modeRampColor = "yellow"
			SetLightColor aRampLights(bRndModeShot), modeRampColor, 2
			ModeCountdown = 50 ' seconds
			If bHardMode Then ModeCountdown = 30
			If bHardMode=False Then
				For each a in aerosmithLights
					SetLightColor a, "orange", 2
				Next
			End If
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_same)
			RefreshPlayerMode
        Case 3:  ' sweet
			DMD Line1, CL(0, ModeNamesShort(PlayerMode)), "", eNone, eNone, eNone, time, False, "":I18.State = 2   ' flash CIU, solid box, elevator, lock
			modeRampColor = "red"
			SetLightColor I35, modeRampColor, 2
			SetLightColor I81, modeRampColor, 2
			SetLightColor I86, modeRampColor, 2
			SetLightColor I91, modeRampColor, 2
			SetLightColor I97, modeRampColor, 2
			SetLightColor I103, modeRampColor, 2
			ModeCountdown = 50 ' seconds
			If bHardMode Then ModeCountdown = 30
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_sweet)
			RefreshPlayerMode
        Case 4: ' dude
			DMD Line1, CL(0, ModeNamesShort(PlayerMode)), "", eNone, eNone, eNone, time, False, "":I19.State = 2   ' solid center, solid elevator
			modeRampColor = "blue"
			SetLightColor I86, modeRampColor, 1
			SetLightColor I91, modeRampColor, 1
			ModeCountdown = 50 ' seconds
			If bHardMode Then ModeCountdown = 30
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_dude)
			'pStartBackLoop "StartGame", "dude.mp4"
			RefreshPlayerMode
        Case 5: ' back
			DMD Line1, CL(0, ModeNamesShort(PlayerMode)), "", eNone, eNone, eNone, time, False, "":I20.State = 2   '  flash right orbit
			modeRampColor = "purple"
			SetLightColor I107, modeRampColor, 2
			ModeCountdown = 50 ' seconds
			If bHardMode Then ModeCountdown = 30
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_back)
			'pStartBackLoop "StartGame", "Back.mp4"
			RefreshPlayerMode
        Case 6:  ' rats
			DMD Line1, CL(0, ModeNamesShort(PlayerMode)), "", eNone, eNone, eNone, time, False, "":I21.State = 2   '  flash center ramp  -- then it lights elevator and toys
			modeRampColor = "red"
			SetLightColor I86, modeRampColor, 2
			ModeCountdown = 50 ' seconds
			If bHardMode Then ModeCountdown = 30
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_rats)
			'pStartBackLoop "StartGame", "Rats.mp4"
			RefreshPlayerMode
    End Select

	If PlayerMode <> -1 Then
		SetBackglassTimer(ModeCountdown)
	End If
	D "UpdatePlayermode calling SetModeLights"
	SetModeLights
End Sub

Sub SetModeLights
	D "SetModeLights"
	' Of any of the modes are 1/2 done then Flash
	if bWizardMode Then Exit Sub

	modesStarted=0
	modesCompleted=0
	SetModeLightComplete 0, I15
	SetModeLightComplete 1, I16
	SetModeLightComplete 2, I17
	SetModeLightComplete 3, I18
	SetModeLightComplete 4, I19
	SetModeLightComplete 5, I20
	SetModeLightComplete 6, I21
	SetModeLightComplete 7, I22
	SetModeLightComplete 8, I23
	D "SetModeLights Result: " & modesStarted

	If modesStarted >=3 and PlayerState(CurrentPlayer).bExtraball_2 = False  then ' started 3 modes 
		PlayerState(CurrentPlayer).bExtraball_2 = True
		setExtraBallLight(True)
	End If 
	If modesCompleted >=6 and PlayerState(CurrentPlayer).bExtraball_3 = False  then ' Finished 6 modes 
		PlayerState(CurrentPlayer).bExtraball_3 = True
		setExtraBallLight(True)
	End If 
End Sub

Sub RefreshPlayerMode()
	D "RefreshPlayerMode :" & bPlayerModeSelect
	If bUsePUPDMD then 
		Select Case PlayerMode
		Case -1:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress8-0.png",0,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 0:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress8-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 1:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress10-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 2:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress10-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 3:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress12-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 4:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress10-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 5:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress8-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 6:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress8-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		End Select
	End If 
	If bUsePUPDMD Then
		PuPlayer.LabelNew pBackglass,"CoinImage",DMDScrFont,		7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1
		PuPlayer.LabelSet pBackglass,"CoinImage","PupOverlays\\Coin.png",1,"{'mt':2,'width':100,'height':100,'xpos':0,'ypos':0}" 

		PuPlayer.LabelNew pBackglass,"JesterImage",DMDScrFont,		7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1	
		PuPlayer.LabelSet pBackglass,"JesterImage","PupOverlays\\Jester.png",1,"{'mt':2,'width':100,'height':100,'xpos':0,'ypos':0}"

		PuPlayer.LabelNew pBackglass,"DiceImage",DMDScrFont,		7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1
		PuPlayer.LabelSet pBackglass,"DiceImage","PupOverlays\\Dice.png",1,"{'mt':2,'width':100,'height':100,'xpos':0,'ypos':0}"
	End If
End Sub

Function CheckWizardModeStart(trigger_name)  'mmmm
	Dim a,i,b
	D "CheckWizardModeStart :" & trigger_name
	CheckWizardModeStart=False
	If trigger_name="I35" Then  ' I34 should already be blinking
		If bMedleyTourReady Then
			D "Start MedleyTour - bMedleyTourReady already set"
			ScoopDelay=20000
			StopScoopLightSeq
			setModeSelectLight(False)
			SetLightColor I34, "white", 0   
			SetLightColor I35, "white", 0
			ModecountdownTimer.UserValue = 0
			ModecountdownTimer.Enabled = False

			aSaveLights()
			for each a in saveLights
				SetLightColor a, "white", 0
			Next

			tmrMedleyTour.Enabled=True
			bMedleyTourReady = False
			bMedleyTourCount=0
			QueueScene "SceneClearPlayMessage '", 0, 1
			QueueScene "SceneClearPlayMessage '", 0, 2

			if bUsePUPDMD then
				StartWizardModeBonus()
			Else
				DisplayDMDText2 "MEDLEY TOUR","WIZARD MODE", 1000, 11, 0
			end if

			vpmtimer.addtimer 18000, "StartWizardModeContinue 3 '"

'If bHardMode=True then you need atleast 1 shot in each mode .. no 0% modes
' turn off all inserts
'ball Save - 40s
'3 ball MB_Multiplier
'blink one mode
'songs played in order of completion in game (v1.07)
'skip songs that were completed to 100% in game mode
'"SONG COMPLETED 1M" - with generic background, 2M for next song (and Shots)
' add a ball upon a song completion


' similar to super modes - show "Loops Needed in background"
'can use smart button
'down to 1 ball return to regular mode and super shots are enabled again
'show mode total at end as well
'upon completion - drain all but 1 ball before returning to regular mode


			bWizardMode = True
			WizardModePoints=0
			WizardBonusPoints=0
			WizardModesComplete=0  	' how many we actual completed
			WizardModeStage=0		' index position in the queue
			PlayerMode3=0
			WizardHits=0  ' for modes that need a required number of hits


'3todo
			' Turn off all the Mode Lights
			I15.State = 0:I16.State = 0:I17.State = 0:I18.State = 0
			I19.State = 0:I20.state = 0:I21.State = 0:I22.State = 0:I23.state = 0

			StackState(kStack_Pri2).Enable(-1)
			if ModePercent(0)>=100 Then SetLightColor I15, "cyan", 1
			if ModePercent(1)>=100 Then SetLightColor I16, "orange", 1
			if ModePercent(2)>=100 Then SetLightColor I17, "yellow", 1
			if ModePercent(3)>=100 Then SetLightColor I18, "red", 1
			if ModePercent(4)>=100 Then SetLightColor I19, "blue", 1
			if ModePercent(5)>=100 Then SetLightColor I20, "purple", 1
			if ModePercent(6)>=100 Then SetLightColor I21, "red", 1
			if ModePercent(7)>=100 Then SetLightColor I22, "green", 1
			if ModePercent(8)>=100 Then SetLightColor I23, "pink", 1

			CheckWizardModeStart = True

			NextWizardStage()
			setMysteryLight(True)
		ElseIf bFinalTourReady Then
			D "Start FinalTour"
			StopScoopLightSeq
			ScoopDelay=3000
			setModeSelectLight(False)
			tmrFinalTour.Enabled=True
			bWizardMode = True
			WizardModePoints=0
			WizardBonusPoints=0
			WizardStagesComplete=0

			if bUsePUPDMD then
				QueueScene "SceneClearMessage '", 0, 1
				QueueScene "ScenePlayMessage ""Video-0x0010.mp4"", """","""","""" '", 5500, 1
				QueueScene "SceneClearPlayMessage '", 0, 1
			Else
				DisplayDMDText2 "FINAL TOUR","WIZARD MODE", 1000, 11, 0
			end if
		
			' Stop ballsaver if it is currently running 
			BallSaverTimerCancel

			' Setup new Ball Saver 
			BallSaverTime = 40						' Note: initially you get 40 seconds
			bBallSaverReady = True

			AddMultiball 6 - BallsOnPlayfield
			bMultiBallMode=True

			StartPlayerModeVideo False
			StackState(kStack_Pri2).Enable(-1)

			For each a in aRampLights				' TODO Set the mode correctly
				SSetLightColor kStack_Pri2, a, "purple", 2
				a.UserValue = 0
			Next   
			CheckWizardModeStart = True
		End If
	End If
End Function

Sub StartWizardModeContinue(nballs)	' delayed until vid and scoop eject
	' Stop ballsaver if it is currently running 
	BallSaverTimerCancel

	' Setup new Ball Saver 
	BallSaverTime = 10						' Note: initially you get 40 seconds   TODO
	bBallSaverReady = True
	AddMultiball nballs - BallsOnPlayfield
	bMultiBallMode=True

	D "remove the timer and bubble"
	PuPlayer.LabelSet pBackglass,"Time", " ",0,""
	pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1)))
End Sub

Dim WizardStagesComplete, WizardModesComplete, PlayerMode3, WizardModeStage, WizardHits

Sub NextWizardStage()
	Dim a,i
	' find first mode in ModeOrder that is not done
	D "NextWizardStage Completed:" & WizardModeStage 

	Do Until WizardModeStage >= 9
		if ModePercent(ModeOrder(WizardModeStage)) < 100 Then	' Dont have to do wizard modes on stages already completed in regular play
			D2 "Found InComplete ModeOrder()=" & ModeOrder(WizardModeStage)
			Exit Do
		End If
		WizardModeStage=WizardModeStage+1
	Loop

	If WizardModeStage=9 then
		D "Completed Wizard Mode"
		EndWizardMode(1)   	
		SetLightColor I35, "white", 2
		bFinalTourReady=True  ' todo not sure on this
	Else
		D "Starting stage=" & WizardModeStage & " PlayerMode3=Mode(x)=" & ModeOrder(WizardModeStage)
		PlayerMode3=ModeOrder(WizardModeStage)
		SetWizardMode()
	End If
End Sub

Sub SetWizardMode()
	Dim a, Line1, time
	D "SetWizardMode PlayerMode3=" & PlayerMode3
	time=1000

	For each a in aRampLights				
		SSetLightColor kStack_Pri2, a, "white", 0
		a.UserValue = 0
	Next
	for each a in aerosmithLights
		SetLightColor a, "orange", 0
	Next
	SetLightColor F141, "red", 0
	SetLightColor F144, "purple", 0

	SetLightColor I75,  "white",0
	SetLightColor I76,  "white",0
	SetLightColor I77,  "white",0
	SetLightColor I78,  "white",0
	SetLightColor I79,  "white",0
	SetLightColor I80,  "white",0
	SetLightColor F152, "white",0  'dude
	SetLightColor F153, "white",0
	SetLightColor F154, "white",0

	
	Line1 = "Medley Wizard Mode"

'same - 9 flashing targets that go out as they are Hit, Score each target regardless If lit
'toys - shoot toybox 6 times , flashing to solid, CR and Elev solid - these shots just go out
'last - 2 shots - not extinguished - flash
'walk - 4 shots - not extinguised except when down to 1 - flash then 1 solid
'sweet - 6 shots - extinguished - solid
'dude - 1 scoop, 3 lanes - shots are extinguished - solid
'back - 25 pop hits
'rats - 25 spinners
'love - 3 shots - not exitinguished - flash

	StartPlayerModeVideo False
	PlaySong "Song-" & PlayerMode3 & ".mp3"

	select case PlayerMode3
		case 0: ' last
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I15.State = 2 ' orbits
			SSetLightColor kStack_Pri2, I62, "cyan", 2
			SSetLightColor kStack_Pri2, I107, "cyan", 2
			WizardHits=2
		case 1: ' walk
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I16.State = 2 ' orbits and ramps
			SSetLightColor kStack_Pri2, I62, "orange", 2
			SSetLightColor kStack_Pri2, I107, "orange", 2
			SSetLightColor kStack_Pri2, I86, "orange", 2
			SSetLightColor kStack_Pri2, I103, "orange", 2
			WizardHits=4
		case 2: ' same
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I17.State = 2 ' aerosmith targets
			for each a in aerosmithLights
				SetLightColor a, "orange", 2
			Next
			WizardHits=9
		case 3: ' sweet
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I18.State = 2 ' shots no orbits
			SSetLightColor kStack_Pri2, I35, "red", 2
			SSetLightColor kStack_Pri2, I81, "red", 2
			SSetLightColor kStack_Pri2, I86, "red", 2
			SSetLightColor kStack_Pri2, I91, "red", 2
			SSetLightColor kStack_Pri2, I97, "red", 2
			SSetLightColor kStack_Pri2, I103, "red", 2
			WizardHits=6
		case 4: ' dude
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I19.State = 2 ' scoop and dude lights
			SSetLightColor kStack_Pri2, I35, "blue", 2
			SetLightColor F152, "blue", 2
			SetLightColor F153, "blue", 2
			SetLightColor F154, "blue", 2
			WizardHits=4
		case 5: ' back
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I20.State = 2 ' pops
			SetLightColor F144, "purple", 2
			WizardHits=25
		case 6: ' rats
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I21.State = 2 ' spinners
			SetLightColor F141, "red", 2
			WizardHits=25
		case 7: ' toys
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I22.State = 2 ' toybox
			SetLightColor I75, "green", 2
			SetLightColor I76, "green", 2
			SetLightColor I77, "green", 2
			SetLightColor I78, "green", 2
			SetLightColor I79, "green", 2
			SetLightColor I80, "green", 2
			SetLightColor I86, "green", 1
			SetLightColor I91, "green", 1
			SSetLightColor kStack_Pri2, I86, "green", 1
			SSetLightColor kStack_Pri2, I91, "green", 1
			SetLightColor F147, "white",2 'toybox
			WizardHits=6
		case 8: ' love
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I23.State = 2 ' elevator
			SSetLightColor kStack_Pri2, I91, "pink", 2
			WizardHits=3
	End Select
End Sub

'4todo
Sub CheckWizardModeProgress(trigger_name)
	dim a,b,i
	Dim bValidHit
	dim bFinalShot
	dim hitLight
	Dim holdScore

	D "CheckWizardModeProgress " & trigger_name & " Mode:" & PlayerMode3 & " Complete:" & WizardModesComplete

	if bDebounce then exit sub 

	If tmrMedleyTour.Enabled Then
		Select case PlayerMode3
			case 0: ' last
				if trigger_name = "I62" and StackState(kStack_Pri2).GetArrowState(I62) <> 0 Then
					SSetLightColor kStack_Pri2, I62, "white", 0
					AddScore WizardModesComplete*1000000
					WizardHits=WizardHits-1
				End if
				if trigger_name = "I107" and StackState(kStack_Pri2).GetArrowState(I107) <> 0 Then
					SSetLightColor kStack_Pri2, I107, "white", 0
					AddScore WizardModesComplete*1000000
					WizardHits=WizardHits-1
				End if
				if WizardHits <= 0 then
					I15.state=1:ModePercent(PlayerMode3)=100
					EndWizardStage()
				End if
			case 1: ' walk
				if trigger_name = "I62"  and StackState(kStack_Pri2).GetArrowState(I62)  <> 0 Then WizardHits=WizardHits-1:AddScore WizardModesComplete*1000000
				if trigger_name = "I107" and StackState(kStack_Pri2).GetArrowState(I107) <> 0 Then WizardHits=WizardHits-1:AddScore WizardModesComplete*1000000
				if trigger_name = "I86"  and StackState(kStack_Pri2).GetArrowState(I86)  <> 0 Then WizardHits=WizardHits-1:AddScore WizardModesComplete*1000000
				if trigger_name = "I103" and StackState(kStack_Pri2).GetArrowState(I103) <> 0 Then WizardHits=WizardHits-1:AddScore WizardModesComplete*1000000
				If WizardHits <= 0 Then
					I16.state=1:ModePercent(PlayerMode3)=100
					EndWizardStage()
				End If
			case 2: ' same
				For each a in aerosmithLights
					If a.name=trigger_name and a.state=2 Then
						a.state=0:AddScore WizardModesComplete*1000000
						WizardHits=WizardHits-1
						exit for
					End If
				Next
				If WizardHits <= 0 Then		
					I17.state=1:ModePercent(PlayerMode3)=100
					EndWizardStage()
				End If
			case 3: ' sweet
				if trigger_name = "I35" and StackState(kStack_Pri2).GetArrowState(I35) <> 0 Then
					SSetLightColor kStack_Pri2, I35, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if trigger_name = "I81" and StackState(kStack_Pri2).GetArrowState(I81) <> 0 Then
					SSetLightColor kStack_Pri2, I81, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if trigger_name = "I86" and StackState(kStack_Pri2).GetArrowState(I86) <> 0 Then
					SSetLightColor kStack_Pri2, I86, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if trigger_name = "I91" and StackState(kStack_Pri2).GetArrowState(I91) <> 0 Then
					SSetLightColor kStack_Pri2, I91, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if trigger_name = "I97" and StackState(kStack_Pri2).GetArrowState(I97) <> 0 Then
					SSetLightColor kStack_Pri2, I97, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if trigger_name = "I103" and StackState(kStack_Pri2).GetArrowState(I103) <> 0 Then
					SSetLightColor kStack_Pri2, I103, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if WizardHits <= 0 Then
					I18.state=1:ModePercent(PlayerMode3)=100
					EndWizardStage()
				End If
			case 4: ' dude
				If trigger_name = "I35" and StackState(kStack_Pri2).GetArrowState(I35) <> 0 Then
					SSetLightColor kStack_Pri2, I35, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if trigger_name = "sw2" and F152.state<>0 Then
					F152.state=0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End If
				if trigger_name = "sw3" and F153.state<>0 Then
					F153.state=0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End If
				if trigger_name = "sw5" and F154.state<>0 Then
					F154.state=0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End If
				If WizardHits <= 0 Then
					I19.state=1:ModePercent(PlayerMode3)=100
					EndWizardStage()
				End If
			case 5: ' back
				If trigger_name = "bumper" Then
					AddScore WizardModesComplete*100000
					WizardHits=WizardHits-1
					if WizardHits <= 0 Then	
						I20.state=1:ModePercent(PlayerMode3)=100
						EndWizardStage()
					End If
				End If
			case 6: ' rats
				If trigger_name = "sw38" Then
					AddScore WizardModesComplete*100000
					WizardHits=WizardHits-1
					if WizardHits <= 0 Then	
						I21.state=1:ModePercent(PlayerMode3)=100
						EndWizardStage()
					End If
				End If
			case 7: ' toys
				if trigger_name = "I86" and StackState(kStack_Pri2).GetArrowState(I86) <> 0 Then
					SSetLightColor kStack_Pri2, I86, "white", 0
					AddScore WizardModesComplete*1000000
				end if
				if trigger_name = "I91" and StackState(kStack_Pri2).GetArrowState(I91) <> 0 Then
					SSetLightColor kStack_Pri2, I91, "white", 0
					AddScore WizardModesComplete*1000000
				end if
				If trigger_name = "I81" Then
					If I75.state=2 Then
						I75.state=1:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
					Elseif I76.state=2 Then
						I76.state=1:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
					Elseif I77.state=2 Then
						I77.state=1:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
					Elseif I78.state=2 Then
						I78.state=1:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
					Elseif I79.state=2 Then
						I79.state=1:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
					Elseif I80.state=2 Then
						I80.state=1:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
					End If
				End If
				If WizardHits <= 0 Then
					I22.state=1:ModePercent(PlayerMode3)=100
					EndWizardStage()  
					I75.state=0:I76.state=0:I77.state=0:I78.state=0:I79.state=0:I80.state=0
					SetLightColor F147, "white",0 'toybox
				End If
			case 8: ' love
					If trigger_name = "I91" and StackState(kStack_Pri2).GetArrowState(I91)<>0 Then
						WizardHits=WizardHits-1:AddScore WizardModesComplete*1000000
					End If
					if WizardHits <= 0 Then	
						I23.state=1:ModePercent(PlayerMode3)=100
						EndWizardStage()
					End If
		End Select

	Elseif tmrFinalTour.Enabled Then
	End If
End Sub

Sub EndWizardStage()
	Dim ScoreVal
	
	WizardModesComplete=WizardModesComplete+1  ' # of stages we actually completed no freebies 
	WizardModeStage=WizardModeStage+1
	D "SONGS COMPLETE " & WizardModesComplete & " Mode:" & PlayerMode3 & " %" & ModePercent(PlayerMode3)
	ScoreVal=1000000*WizardModesComplete
	AddScore ScoreVal
	QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", ""SONG COMPLETE"","""&FormatScore(ScoreVal)&""","""" '", 1300, 2
	QueueScene "SceneClearMessage '", 0, 2
	If BallsOnPlayfield <= 6 Then
		AddMultiball 1
	End If
	NextWizardStage()
End Sub

'5todo
Sub EndWizardMode(mode)
	Dim i,a
	D "EndWizardMode Mode:" & mode 
	If tmrMedleyTour.Enabled Then
		D "EndWizardMode .. was in Medley Mode"
		tmrMedleyTour.Enabled=False

		for each a in aRampLights
			SSetLightColor kStack_Pri2, a, "white", 0
			SSetLightColor kStack_Pri0, a, "white", 0   ' should be off anyways prior to Medley Tour
		Next
		StackState(kStack_Pri2).Disable 

		PlaySong "Song-" & SaveMode & ".mp3"

		aRestoreLights()

		playclear pBackglass
		playmedia "Video-0x0000.mp4", "PupVideos", pBackglass, "", -1, "", 1, 1

		bMedleyTourDone=True
		ShowPlayerModeComplete(2)		' Show Mode total
		bWizardMode = False 

		' Turn off all the Mode Lights
		I15.State = 0:I16.State = 0:I17.State = 0:I18.State = 0
		I19.State = 0:I20.state = 0:I21.State = 0
		D "StopPlayerMode calling SetModeLights"
		SetModeLights()			' Set mode lights based on progress incl wizard mode completions

		Playermode3 = -1
		
		CheckWizardModesReady

		For i = 0 to 6	' check if one of the modes is not completed then allow them to select
			if ModePercent(i) < 100 then
				D "Still another song available: ModePercent C i=" & i & " %" & ModePercent(i)
				setModeSelectLight(True)
				exit For
			end If
		Next
	Elseif tmrFinalTour.Enabled Then
		D "EndWizardMode .. was in Final Tour Mode"
	Else
		D "No wizard mode active"
	End If
End Sub

Dim WizardModeBonusCnt

Sub StartWizardModeBonus()
	tmrWizardModeBonus.Interval=200
	tmrWizardModeBonus.UserValue=-1
	tmrWizardModeBonus.Enabled=True
	WizardModeBonusCnt=0
End Sub


Sub tmrWizardModeBonus_Timer()
	Dim ScoreVal
	tmrWizardModeBonus.Interval=3000
	ScoreVal=10000000

	D "tmrWizardModeBonus " & tmrWizardModeBonus.UserValue

	Select Case tmrWizardModeBonus.UserValue
		case -1:
			tmrWizardModeBonus.Interval=5800  ' Wizard Medley Screen
			QueueFlush()
			GiEffect 3
			LightEffect 3
			QueueScene "ScenePlayMessage ""Video-0x003F.mp4"", """","""","""" '", 5500, 1
			QueueScene "SceneClearPlayMessage '", 0, 1
			tmrWizardModeBonus.UserValue=0
			D "ChangeVol pBackglass Volume for Callouts"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":60 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic&",     ""FN"":11, ""VL"":60 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pAudio&",     ""FN"":11, ""VL"":60 }"
		Case 0:
			D "Last Child Bonus"
			if ModePercent(0) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(0)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				AddScore ScoreVal
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0459Last.mp3",80, 1
				'playmedia "","audio-modes",pCallouts,"Sound-0x0459Last.mp3",1000,"",1,1 
			End If
			tmrWizardModeBonus.UserValue=1

		Case 1:
			D "Walk This Way Bonus"
			if ModePercent(1) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(1)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1		
				AddScore ScoreVal
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0458Walk.mp3",80, 1
				'playmedia "","audio-modes",pCallouts,"Sound-0x0458Walk.mp3",1000,"",1,1  ' 
			End If

			tmrWizardModeBonus.UserValue=2
		Case 2:
			D "Same Old Song Bonus"
			if ModePercent(2) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(2)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				AddScore ScoreVal
				tmrWizardModeBonus.Interval=3000
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0457Same.mp3",80, 1
				'playmedia "","audio-modes",pCallouts,"Sound-0x0457Same.mp3",1000,"",1,1  ' 
			End If
			tmrWizardModeBonus.UserValue=3
		Case 3:
			D "Sweet Emotion Bonus"
			if ModePercent(3) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(3)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				AddScore ScoreVal
			'TODOFIX	PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0456Sweet.mp3",80, 1
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0457Same.mp3",80, 1
			End If
			tmrWizardModeBonus.UserValue=4
		Case 4:
			D "Dude Bonus"
			if ModePercent(4) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(4)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				AddScore ScoreVal
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0453Dude.mp3",80, 1
				'playmedia "","audio-modes",pCallouts,"Sound-0x0453Dude.mp3",1000,"",1,1  '
			End If
			tmrWizardModeBonus.UserValue=5
		Case 5:
			D "Back in the Saddle Bonus"
			if ModePercent(5) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(5)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				AddScore ScoreVal
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0454Back.mp3",80, 1
				'playmedia "","audio-modes",pCallouts,"Sound-0x0454Back.mp3",1000,"",1,1  ' 
			End If
			tmrWizardModeBonus.UserValue=6
		Case 6:
			D "Rats in the Cellar Bonus"
			if ModePercent(6) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(6)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				AddScore ScoreVal
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0452Rats.mp3",80, 1
				'playmedia "","audio-modes",pCallouts,"Sound-0x0452Rats.mp3",1000,"",1,1  '
			End If
			tmrWizardModeBonus.UserValue=7
		Case 7:
			D "Toys in the Attic Bonus"
			if ModePercent(7) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(7)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				AddScore ScoreVal
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0450Toys.mp3",80, 1
				'playmedia "","audio-modes",pCallouts,"Sound-0x0450Toys.mp3",1000,"",1,1  '
			End If
			tmrWizardModeBonus.UserValue=8
		Case 8:
			D "Love in an Elevator Bonus"
			if ModePercent(8) < 100 Then
				' Nothing
				QueueScene "SceneClearMessage '", 0, 1
			Else
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(8)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				QueueScene "SceneClearMessage '", 0, 1
				AddScore ScoreVal
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0451Love.mp3",80, 1
			End If
			tmrWizardModeBonus.Enabled=False
			turnitbackup()

	End Select
	D "tmrWizardModeBonus Finish Interval:" & tmrWizardModeBonus.Interval
End Sub
'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub table1_Exit
	Savehs
	If b2son then Controller.Stop
	If bUseUltraDMD Then FlexDMD.Run = False
'	objFile.close
End Sub

'********************
'     Flippers
'********************
Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
    If Enabled Then
		LF.fire 
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
		RotateLaneLightsLeft
    Else
        LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub


Sub SolRFlipper(Enabled)
    If Enabled Then
        RF.fire 
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
        RotateLaneLightsRight
    Else
        RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
    End If
End Sub


' flippers hit Sound

Sub LeftFlipper_Collide(parm)
	If bUseUltraDMD then AddScore 1	' Keep the display from blanking
    LeftFlipperCollide parm
End Sub


Sub RightFlipper_Collide(parm)
	If bUseUltraDMD then AddScore 1	' Keep the display from blanking
    RightFlipperCollide parm
End Sub


Sub RotateLaneLightsLeft()
	Dim TempState
	TempState = F152.State
	F152.State = F153.State
	F153.State = F154.State
	F154.State = TempState
End Sub

Sub RotateLaneLightsRight()
	Dim TempState
	TempState = F154.State
	F154.State = F153.State
	F153.State = F152.State
	F152.State = TempState
End Sub

'*********
' Queue - This could be used for anything but I use it to queue  priority=1 items up with the option to have 1 Priority=2 item queued or running 
'				Thought here is Pri 1 items need to be shown, Pri 2 items can be shown If an item is running 
'
' 	NOTE - Since VPMtimer is limited to 20 concurrent timers you need a timer called tmrQueue to ensure items dont get dropped 
'	QueueScene
'		Command=vbscript command    ex:   "RunFunction ""Test"", 123  '"
'		Length=milliseconds before running the next item in the queue
'		Priority=Number, 0 being highest
'
'*********
Dim PupQueue(20, 3)			' Size=20,  Fields are 0=Command, 1=Priority, 2=time
Dim PupQueueEndPos			' Size of the queue (-1 = Empty)
Dim QueueActive				' We are actively running something 
Dim QueueCurrentTime		' How much time is this one going to run (Just used for gtting the queue time)
QueueActive=False
PupQueueEndPos=-1
Sub QueueFlush()
	PupQueueEndPos=-1
	QueueActive=False
End Sub
Function getQueueTime()		' Returns how much time left on queue
	Dim time,i 
	time = 0
	D2 "GetQueueTime:" & now 
	D2 "GetQueueTime:" & QueueCurrentTime & " " & QueueActive
	If QueueActive and QueueCurrentTime <> 0 then time = (DateDiff("s", now, QueueCurrentTime) * 1000)
	D2 "GetQueueTime Active:" & time

	for i = 0 to PupQueueEndPos
		time = time + PupQueue(i, 2) 
	Next
	getQueueTime = time
	D2 "GetQueueTime ret:" & time
End Function


Sub QueuePop()
	If PupQueueEndPos = -1 then exit sub 
	PupQueue(0, 1 )=99
	SortPupQueue
	PupQueue(PupQueueEndPos,0 )=""
	PupQueue(PupQueueEndPos,2 )=0
	PupQueueEndPos=PupQueueEndPos-1

	D2 "--Q-Dump Pop---"
	Dim xx
	for xx = 0 to PupQueueEndPos
		D2 xx & " " & PupQueue(xx, 0) & " " & PupQueue(xx, 1) & " " & PupQueue(xx, 2) 
	Next 
	D2 "--Q-Dump Pop---"
End Sub
 
Sub QueueScene(Command, msecLen, priority) 
	D2 "Queue Scene " & Command & " Len: " & msecLen
	
	If PupQueueEndPos < UBound(PupQueue, 1) then 
		PupQueueEndPos=PupQueueEndPos+1
	End If 
	' NOTE: If it is full we overwrite the lowest priority (Optionally we could make the queue bigger)
	PupQueue(PupQueueEndPos,0 )=Command
	PupQueue(PupQueueEndPos,1 )=priority
	PupQueue(PupQueueEndPos,2 )=msecLen
	SortPupQueue
	
	D2 "v--Q-Dump---"
	Dim xx
	for xx = 0 to PupQueueEndPos
		I xx & " " & PupQueue(xx, 0) & " " & PupQueue(xx, 1) & " " & PupQueue(xx, 2) 
	Next 
	D2 "^--Q-Dump---"

	RunQueue True
End Sub

Sub tmrQueue_Timer
	tmrQueue.Enabled = False 
	RunQueue False
End Sub 

Sub RunQueue(bNewItem)
	dim qCmd, qTime
	D2 "Run Queue " & QueueActive & " " & bNewItem & " " & Now
	If QueueActive = False or bNewItem=False then 	' Nothing is running Or we just finished running something 
		If PupQueueEndPos <> -1 then
			QueueActive = True
			qCmd=PupQueue(0, 0)
			qTime=PupQueue(0, 2)
			D2 "Exec " & qCmd
			Execute qCmd
			D2 "Timer " & qTime
			If qTime > 0 then 
				QueueCurrentTime = DateAdd("s",qTime/1000, now)
				D2 QueueCurrentTime
				tmrQueue.Interval = qTime
				tmrQueue.Enabled = True
				'vpmtimer.addtimer cInt(qTime), "RunQueue False '"
				QueuePop
			Else			' No timer just run the next item in the queue 
				QueueCurrentTime = 0
				QueuePop
				RunQueue False
			End If
		Else 
			D2 "Queue Empty Deactivated"
			QueueActive = False
		End If 
	End if
End Sub

Sub SortPupQueue
	dim a, j, temp1, temp2, temp3
	for a = PupQueueEndPos - 1 To 0 Step -1
		for j= 0 to a
			If PupQueue(j, 1)>PupQueue(j+1, 1) then
				temp1=PupQueue(j+1,0 )
				temp2=PupQueue(j+1,1 )
				temp3=PupQueue(j+1,2 )
				PupQueue(j+1,0 )=PupQueue(j,0 )
				PupQueue(j+1,1 )=PupQueue(j,1 )
				PupQueue(j+1,2 )=PupQueue(j,2 )
				PupQueue(j, 0 )=temp1
				PupQueue(j, 1 )=temp2
				PupQueue(j, 2 )=temp3
			end if
		next
	next 
End Sub


'*********
' TILT
'*********

Dim PauseBigScore:PauseBigScore=False
Dim TiltDangerWait
TiltDangerWait=False
Sub SceneTilt(Message)
	PauseBigScore=True
	TiltDangerWait=True
	playmedia "Video-0x0047.mp4", "PupBackgrounds", pOverVid, "", -1, "", 1, 1
	PuPlayer.LabelShowPage pOverVid, 1,0,""
	If TiltCount(CurrentPlayer) = 1 then 
		PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""
		puPlayer.LabelSet pOverVid,"OverMessage1", ""	,1,""
		puPlayer.LabelSet pOverVid,"OverMessage2", Message	,1,""
		puPlayer.LabelSet pOverVid,"OverMessage3", ""	,1,""
	Else 
		PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""
		puPlayer.LabelSet pOverVid,"OverMessage1", ""	,1,""
		puPlayer.LabelSet pOverVid,"OverMessage2", Message	,1,""
		puPlayer.LabelSet pOverVid,"OverMessage3", ""	,1,""
	End If 
End Sub


Sub SceneClearTilt()
	TiltDangerWait=False
	puPlayer.LabelSet pOverVid,"OverMessage1", " "	,0,""
	puPlayer.LabelSet pOverVid,"OverMessage2", " "	,0,""
	puPlayer.LabelSet pOverVid,"OverMessage3", " "	,0,""
	playclear pOverVid
	PauseBigScore=False
End Sub


Sub SceneClearPlayMessage()
	PuPlayer.LabelSet pOverVid,"OverMessage1"," ",0,""
	PuPlayer.LabelSet pOverVid,"OverMessage2"," ",0,""
	PuPlayer.LabelSet pOverVid,"OverMessage3"," ",0,""
	playclear pOverVid
	PauseBigScore=False
End Sub


Sub ScenePlayMessage(Media, Message1, Message2, Message3)
	PauseBigScore=True
	PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""
	If Media <> "" 	Then playmedia Media, "PupVideos", pOverVid, "", -1, "", 1, 1
	PuPlayer.LabelShowPage pOverVid, 1,0,""

	If Len(Message1) > 15 then  
		PuPlayer.LabelSet pOverVid,"OverMessage1", Message1	,1,"{'mt':2,'size': " & 8*FontScale & " }"   
	ElseIf Len(Message1) > 10 then
		PuPlayer.LabelSet pOverVid,"OverMessage1", Message1	,1,"{'mt':2,'size': " & 12*FontScale & " }" 
	Else
		PuPlayer.LabelSet pOverVid,"OverMessage1", Message1	,1,"{'mt':2,'size': " & 15*FontScale & " }" 
	End if

	If Len(Message2) > 9 then  
		PuPlayer.LabelSet pOverVid,"OverMessage2", Message2	,1,"{'mt':2,'size': " & 8*FontScale & " }"   
	Else
		PuPlayer.LabelSet pOverVid,"OverMessage2", Message2	,1,"{'mt':2,'size': " & 10*FontScale & " }" 
	End if

	puPlayer.LabelSet pOverVid,"OverMessage3", Message3	,1,""
End Sub 

Sub SceneClearMessage()
	PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""
	PuPlayer.LabelSet pBackglass,"Message1"," ",0,""
	PuPlayer.LabelSet pBackglass,"Message2"," ",0,""
	PuPlayer.LabelSet pBackglass,"Message3"," ",0,""
	PauseBigScore=False
End Sub


Sub SceneMessage(Message1, Message2, Message3)
	D "SceneMessage " & Message1
	PauseBigScore=True
	PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""

	If Len(Message1) > 15 then
		PuPlayer.LabelSet pBackglass,"Message1", Message1	,1,"{'mt':2,'size': " & 10*FontScale & " }"   
	ElseIf Len(Message1) > 10 then
		PuPlayer.LabelSet pBackglass,"Message1", Message1	,1,"{'mt':2,'size': " & 12*FontScale & " }" 
	Else
		PuPlayer.LabelSet pBackglass,"Message1", Message1	,1,"{'mt':2,'size': " & 15*FontScale & " }" 
	End if

	If Len(Message2) > 9 then
		PuPlayer.LabelSet pBackglass,"Message2", Message2	,1,"{'mt':2,'size': " & 12*FontScale & " }"   
	Else
		PuPlayer.LabelSet pBackglass,"Message2", Message2	,1,"{'mt':2,'size': " & 18*FontScale & " }" 
	End if

	puPlayer.LabelSet pBackglass,"Message3", Message3	,1,""
End Sub 

Sub SceneBMessage(Message1, Message2, Message3)  ' Super Bonus Messages   - All 1 Color
	D "SceneMessage " & Message1
	PauseBigScore=True
	PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""

	If Len(Message1) > 15 then
		PuPlayer.LabelSet pBackglass,"Message1", Message1	,1,"{'mt':2,'size': " & 10*FontScale & " }"   
	ElseIf Len(Message1) > 10 then
		PuPlayer.LabelSet pBackglass,"Message1", Message1	,1,"{'mt':2,'size': " & 12*FontScale & " }" 
	Else
		PuPlayer.LabelSet pBackglass,"Message1", Message1	,1,"{'mt':2,'size': " & 15*FontScale & " }" 
	End if

	If Len(Message2) > 9 then
		PuPlayer.LabelSet pBackglass,"Message2", Message2	,1,"{'mt':2,'size': " & 12*FontScale & " }"   
	Else
		PuPlayer.LabelSet pBackglass,"Message2", Message2	,1,"{'mt':2,'size': " & 18*FontScale & " }" 
	End if

	puPlayer.LabelSet pBackglass,"Message3", Message3	,1,""
End Sub 

Sub TransScenePlayMessage(Media, Message1, Message2, Message3)   ' transparent video overlay
	playmedia Media, "PupVideos", pPopUp5, "", -1, "", 1, 1
	playmedia "ARS107-Scene-100.mp4", "PupVideos", pPopUp5, "", -1, "", 1, 1
	puPlayer.LabelSet pPopUP5,"TOverMessage1", "XXXXTESTING"""	,1,""
End Sub 


Sub ShowPlayerModeComplete(ExtraMode) ' Show the total for the player mode (ExtraMode: 0=ToyMB, 1-ElevMB, 2=WizardMode, -1=DefaultMode)
	D "ShowPlayerModeComplete " & ExtraMode & " PlayerMode:" & PlayerMode & " bSecondMode:" & bSecondMode
	dim ModeStr
	Dim ScoreVal
	Dim SecondStr
	dim thisMode
	ModeStr=""
	If ExtraMode = 0 Then				' ToyMB
		ModeStr = "TOYBOX MULTIBALL"
		ScoreVal = ToyBoxMBJackpotTotal
	elseIf ExtraMode = 1 Then 			' ElevatorMB
		ModeStr = "ELEVATOR MULTIBALL"
		ScoreVal = ElevMBJackpotTotal

	elseIf PlayerMode > -1 Then
		thisMode = PlayerMode
		ScoreVal=ModePoints
		ModeStr=ModeNames(thisMode)
	elseif ExtraMode= 2 then 
		ScoreVal=WizardModePoints	
		ModeStr = "MEDLEY TOUR"
	elseif ExtraMode = 3 then 
			ModeStr = "FINAL TOUR"
	End If

	If ModeStr <> "" then 
		' Queue up PlayerModeComplete scene
		QueueScene "SceneMessage """&ModeStr&""","""&FormatScore(ScoreVal)&""",""TOTAL"" '", 2000, 2
		QueueScene "SceneClearMessage '", 0, 2

		If ExtraMode = -1 Then
			If PlayerMode <> -1 and bSecondMode=False Then
				If ModePercent(PlayerMode) >= 100 Then
					QueueScene "SceneFinishMode " & PlayerMode & " '", 6000, 2
					QueueScene "SceneClearFinishMode '", 0, 2
				End If
			End If
		elseIf ExtraMode = 0 and I22.state = 1 then				' We completed it 
			QueueScene "SceneFinishMode 7 '", 6000, 2
			QueueScene "SceneClearFinishMode '", 0, 2			
		elseIf ExtraMode = 1 and I23.state = 1 then				' We completed it  
			QueueScene "SceneFinishMode 8 '", 6000, 2
			QueueScene "SceneClearFinishMode '", 0, 2			
		End If 
	End If 
End Sub


Sub SceneClearFinishMode() ' hide all the medley tour texts
	D "Scene Clear"
	PuPlayer.LabelSet pOverVid, "F0", "PuPOverlays\\CompleteLevel-0.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F1", "PuPOverlays\\CompleteLevel-1.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F2", "PuPOverlays\\CompleteLevel-2.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F3", "PuPOverlays\\CompleteLevel-3.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F4", "PuPOverlays\\CompleteLevel-4.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F5", "PuPOverlays\\CompleteLevel-5.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F6", "PuPOverlays\\CompleteLevel-6.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F7", "PuPOverlays\\CompleteLevel-7.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F8", "PuPOverlays\\CompleteLevel-8.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F9", "PuPOverlays\\CompleteLevel-9.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"

	PuPlayer.LabelShowPage pOverVid, 1,0,""
    playclear pOverVid
End Sub


'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round
Sub CheckTilt                                    'Called when table is nudged
    If NOT bGameInPlay Then Exit Sub
	If TiltDangerWait then Exit Sub
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
	If Tilt > 15 and TiltCount(CurrentPlayer) < 2 Then 
		TiltCount(CurrentPlayer) = TiltCount(CurrentPlayer) + 1
		
		QueueScene "SceneTilt ""DANGER"" '", 3000, 1
		QueueScene "SceneClearTilt '", 0, 1
	ElseIf Tilt > 15 Then 'If more that 15 then TILT the table
        Tilted = True
		PuPlayer.playlistplayex pCallouts,"audioevents","tilted-Sound-0x03A8.wav",100, 1
		SceneTilt "TILT"
		vpmtimer.addtimer 3000, "SceneClearTilt '"

        DMDFlush
		DisplayDMDText "TILT","", 3000
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub


Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub


Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart

        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        'clean up the buffer display
        DMDFlush
    End If
End Sub


Sub TiltRecoveryTimer_Timer()
    ' If all the balls have been drained then..
    If(BallsOnPlayfield = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus If the table is tilted)
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub



' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************


Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function


Const tnob = 20 ' total number of balls
Const lob = 0   'number of locked balls
ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol
    BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

    ' exit the sub If no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

	'debug.print lob & " " & UBound(BOT)

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
		'debug.print "b"

		aBallShadow(b).X = BOT(b).X
		aBallShadow(b).Y = BOT(b).Y

		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		'***Ball Drop Sounds***
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
    Next
End Sub


' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
'
Sub AddScore(points)
	dim multiplierVal
	dim tPoints
	ResetBallSearch
    If(Tilted = False)Then
		D2 "AddScore: " & points	
		multiplierVal = Multiplier3x * MultiplierShot * PlayMultiplier
		tPoints=(points * multiplierVal)

		If Mode2Percent(3) <> -1 and Mode2Percent(3) < 100 and PlayerMode2 <> 3 Then
			Mode2Progress(3) = Mode2Progress(3)+1
			Mode2Percent(3) = CINT((Mode2Progress(3)  / 20) * 100)

			tPoints=int(tPoints*1.50)

			ShowPlayerMode2(3)
			If (Mode2Progress(3) >= 20) Then
				Mode2Percent(3) = 100
			End If
		End If
		ModePoints = ModePoints + tPoints
		if bWizardMode then WizardModePoints = WizardModePoints + tPoints

        ' add the points to the current players score variable
        Score(CurrentPlayer) = Score(CurrentPlayer) + tPoints
        ' update the score displays

		If Multiplier3x <> 1 then 
			Multiplier3x = 1	' 3x only last for next score
			SetLightColor I32, "green", I32.UserValue
			SetLightColor I46, "green", I46.UserValue
			SetLightColor I67, "green", I67.UserValue
			SetLightColor I84, "green", I84.UserValue
			SetLightColor I95, "green", I95.UserValue
			SetLightColor I101, "green",I101.UserValue
			SetLightColor I89, "green",  I89.UserValue
			SetLightColor I112, "green",I112.UserValue
		End If
        DMDScore
    End if
End Sub


'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
	' Load Orbital scores 
	GetScores

    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 100000 End If
    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 100000 End If
    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 100000 End If
    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 100000 End If
    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(TableName, "HighScore5Name")
    If(x <> "")then HighScoreName(4) = x Else HighScoreName(4) = "MED" End If
    x = LoadValue(TableName, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0 End If

    'x = LoadValue(TableName, "Jackpot")
    'If(x <> "") then Jackpot = CDbl(x) Else Jackpot = 200000 End If
    x = LoadValue(TableName, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If
HighScore(0)=10
HighScore(1)=8
HighScore(2)=6
HighScore(3)=4
End Sub

Sub Savehs
    SaveValue TableName, "HighScore1", HighScore(0)
    SaveValue TableName, "HighScore1Name", HighScoreName(0)
    SaveValue TableName, "HighScore2", HighScore(1)
    SaveValue TableName, "HighScore2Name", HighScoreName(1)
    SaveValue TableName, "HighScore3", HighScore(2)
    SaveValue TableName, "HighScore3Name", HighScoreName(2)
    SaveValue TableName, "HighScore4", HighScore(3)
    SaveValue TableName, "HighScore4Name", HighScoreName(3)
    SaveValue TableName, "HighScore5", HighScore(4)
    SaveValue TableName, "HighScore5Name", HighScoreName(4)
    SaveValue TableName, "Credits", Credits
    SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Clearhs
	HighScore(0) = 100000
	HighScoreName(0) = "AAA"
	HighScore(1) = 100000
	HighScoreName(1) = "BBB"
	HighScore(2) = 100000
	HighScoreName(2) = "CCC"
	HighScore(3) = 100000
	HighScoreName(3) = "DDD"
    SaveValue TableName, "HighScore1", HighScore(0)
    SaveValue TableName, "HighScore1Name", HighScoreName(0)
    SaveValue TableName, "HighScore2", HighScore(1)
    SaveValue TableName, "HighScore2Name", HighScoreName(1)
    SaveValue TableName, "HighScore3", HighScore(2)
    SaveValue TableName, "HighScore3Name", HighScoreName(2)
    SaveValue TableName, "HighScore4", HighScore(3)
    SaveValue TableName, "HighScore4Name", HighScoreName(3)
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
	D "CheckHighScore"
	osbtempscore = Score(CurrentPlayer)

    If Score(CurrentPlayer) > HighScore(0) Then 'add 1 credit for beating the highscore
        AwardSpecial
    End If
	' Bail out on high score when auto testing 
    If AutoQa=false and AutoAI=false and Score(CurrentPlayer) >= HighScore(3) Then	' Overwrite the lowest
       'vpmtimer.addtimer 2000, "PlaySoundVol ""say-greatScore"", VolDef '"
        HighScore(3) = Score(CurrentPlayer)
        'enter player's name
        HighScoreEntryInit()
    Else
		If osbactive <> 0 then 
			' Submit to Orbital
			osbtemp = osbdefinit
			SubmitOSBScore
		End If 
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
	D "HighScoreEntryInit()"
	If bUsePUPDMD then 
		PuPlayer.LabelShowPage pBackglass, 3,0,""
		pDMDEvent(kDMD_Attract)
		puPlayer.LabelSet pBackglass,"EnterHS1", "PLAYER " & CurrentPlayer+1	,1,""
		puPlayer.LabelSet pBackglass,"EnterHS2", "ENTER INITIALS" ,1,""
	End If 
    hsbModeActive = True
    'laySoundVol "say-EnterYourInitials2", VolDef
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 1

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ<+-0123456789"    ' < is used to delete the last letter
    hsCurrentLetter = 1
    DMD_Clearforhighscore()
	DMDId "hsc", "Enter", "Your Name", 999999
    HighScoreDisplayName()
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        PlaySoundVol "fx_Previous", VolDef
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayName()
    End If

    If keycode = RightFlipperKey Then
        PlaySoundVol "fx_Next", VolDef
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayName()
    End If

    If keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
            PlaySoundVol "fx_Enter", VolDef
			
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3)then
                HighScoreCommitName()
            else
                HighScoreDisplayName()
            end if
        else
			PlaySoundVol "fx_Esc", VolDef
			
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit > 0)then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayName()
        end if
    end if
End Sub


Sub HighScoreDisplayName()
	Dim i, TempStr
	Dim bugFIX

    TempStr = " >"
    if(hsCurrentDigit> 0) then TempStr = TempStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2) then TempStr = TempStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempStr = TempStr & "_"
        else
            TempStr = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) then TempStr = TempStr & hsEnteredDigits(2)

    TempStr = TempStr & "< "
	bugFIX=""
	If bUseUltraDMD then		' Not sure why but ULTRA doesnt update unless both lines change
		If (hsCurrentLetter mod 2) = 0 then 
			bugFIX="-"
		else
			bugFIX=" -"
		End If
	End If
	DMDMod "hsc", "YOUR NAME" & bugFIX, Mid(TempStr, 2, 5), 999999
	If bUsePUPDMD then puPlayer.LabelSet pBackglass,"EnterHS3", TempStr,1,""
End Sub

Sub HighScoreCommitName()
    hsbModeActive = False

    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ")then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
	DMD_Clearforhighscore

	' Submit to Orbital
	osbtemp = hsEnteredName
	SubmitOSBScore

	If bUsePUPDMD then 
		puPlayer.LabelSet pBackglass,"EnterHS1", "",1,""
		puPlayer.LabelSet pBackglass,"EnterHS2", "",1,""
		puPlayer.LabelSet pBackglass,"EnterHS3", "",1,""
	End If 
	pBGGamePlay
    EndOfBallComplete()
End Sub


Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) < HighScore(j + 1)Then
                tmp = HighScore(j + 1)
                tmp2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = tmp
                HighScoreName(j) = tmp2
            End If
        Next
    Next
End Sub


Sub DMD_Clearforhighscore()
	If (bUseUltraDMD) then
		UltraDMD.CancelRendering
		UltraDMD.Clear
	End If
End Sub


Sub AwardSpecial()
	QueueScene "playmedia ""Video-0x0054.mp4"", ""PupVideos"", pOverVid , """", -1, """", 1, 1 '", 5000, 2
	DMD "_", CL(1, ("EXTRA GAME WON")), "_", eNone, eBlink, eNone, 1000, True, ""
	KnockerSolenoid
	Credits = Credits + 1
	DOF 140, DOFOn
	DOF 115, DOFPulse
	GiEffect 1
	LightEffect 1
	DOF 400, DOFPulse   'DOF MX - Special
End Sub


Sub AddBonusMultiplier(n)
    ' If not at the maximum bonus level
    if(BonusMultiplier + n < MaxMultiplier)then
        SetBonusMultiplier(BonusMultiplier + n)
		If bUsePUPDMD then 
			QueueScene "ScenePlayMessage """&"Video-0x006A-" & Cstr(BonusMultiplier) &".mp4"", """","""","""" '", 3000, 2
			QueueScene "SceneClearPlayMessage '", 0, 2
		Else 
			DisplayDMDText2 "BONUS MULTIPLIER",BonusMultiplier & "x", 1000, 11, 0
		End If 
    End if
End Sub


Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier = Level
End Sub


Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
		QueueScene "playmedia ""Video-0x0014.mp4"", ""PupVideos"", pOverVid , """", -1, """", 1, 1 '", 5000, 2
        DMD "_", CL(1, ("EXTRA BALL WON")), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("Knocker_1", 122, DOFPulse, DOFKnocker)
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        GiEffect 1
        LightEffect 2
		SetLightColor I14, "yellow", 2
    END If
End Sub


' *************************************************************************
' ULTRA DMD Instead of the local DMD display
' *************************************************************************

Sub LoadUltraDMD
	Dim WshShell
	Set WshShell = CreateObject("WScript.Shell")
	WshShell.RegWrite "HKCU\Software\UltraDMD\fullcolor",UseFullColor,"REG_SZ"  'UseFullColor
	' FlexDMD API is found at https://github.com/vbousquet/flexdmd/blob/f3d67a6667b8c35637eec0e90a0f23b4647f533b/docs/FlexDMD_API.md 

	On Error Resume Next
	Dim FlexDMD
	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
	FlexDMD.GameName = cGameName   
	FlexDMD.Color = RGB(255,0,255) '&hCA1BCC
	FlexDMD.Width = 128
	FlexDMD.Height = 36
	FlexDMD.Clear=True
	FlexDMD.RenderMode=2
	Set UltraDMD = FlexDMD.NewUltraDMD()

    If UltraDMD is Nothing Then
        MsgBox  "No UltraDMD found.  This table MAY run without it."
		bUseUltraDMD=False
        Exit Sub
    End If
	On Error Goto 0

    UltraDMD.Init
	UltraDMD.SetVideoStretchMode 2
    If Not UltraDMD.GetMajorVersion = 1 Then
        MsgBox  "Incompatible Version of UltraDMD found."
        Exit Sub
    End If

    If UltraDMD.GetMinorVersion < 1 Then
        MsgBox  "Incompatible Version of UltraDMD found. Please update to version 1.1 or newer."
        Exit Sub
    End If

    Dim fso
    Set fso = CreateObject("Scripting.FileSystemObject")
    Dim curDir
    curDir = fso.GetAbsolutePathName(".")
    UltraDMD.SetProjectFolder curDir & "\" & cGameName & ".UltraDMD"
	PlayDMDScene "Video-0x0098.wmv", "LOADING", "GAME", 4000
End Sub

Sub DMDClearQueue				' It looks like If we call this too fast it will cancel the ULTRA DMD logo scene
	If bUseUltraDMD and UltraDMDVideos Then
		If UltraDMD.IsRendering Then
			UltraDMD.CancelRendering
		End If
	End If
End Sub

Sub PlayDMDScene(video, toptext, bottomtext, timeMs)
	If bUseUltraDMD and UltraDMDVideos Then
		debug.print "PlayDMDScene " & video
		UltraDMD.DisplayScene00 video, toptext, 15, bottomtext, 15, UltraDMD_Animation_None, timeMs, UltraDMD_Animation_None
	End If
End Sub

Sub DisplayDMDText(Line1, Line2, duration)
	D ">>>>DMDText " & Line1 & " " & Line2 & " Duration:" & duration
	If bUseUltraDMD Then
		debug.print "Calling UltraDMD"
		UltraDMD.DisplayScene00 "", Line1, 15, Line2, 15, 14, duration, 14
	ElseIf bUsePUPDMD Then
		If bPupStarted then 
			If Line1 = "" or Line1 = "_" then 
				pupDMDDisplay "-", Line2, "" ,Duration/1000, 0, 10
			else
				pupDMDDisplay "-", Line1 & "^" & Line2, "" ,Duration/1000, 0, 10
			End If 
		End If 
	End If
End Sub

Sub DisplayDMDText2(Line1, Line2, duration, pri, blink)
	If bUseUltraDMD Then
		UltraDMD.DisplayScene00 "", Line1, 15, Line2, 15, 14, duration, 14
	ElseIf bUsePUPDMD Then
		If bPupStarted then 
			If Line1 = "" or Line1 = "_" then 
				pupDMDDisplay "-", Line2, "" ,Duration/1000, blink, pri
			else
				pupDMDDisplay "-", Line1 & "^" & Line2, "" ,Duration/1000, blink, pri
			End If 
		End If 
	End If
End Sub


Sub DMDId(id, toptext, bottomtext, duration) 'used in the highscore entry routine
	If bUseUltraDMD then 
		UltraDMD.DisplayScene00ExwithID id, false, "", toptext, 15, 0, bottomtext, 15, 0, 14, duration, 14
	ElseIf bUsePUPDMD Then
		If bPupStarted then pupDMDDisplay "default", toptext & "^" & bottomtext, "" ,Duration/1000, 0, 10
	End If 
End Sub

Sub DMDMod(id, toptext, bottomtext, duration) 'used in the highscore entry routine
	If bUseUltraDMD then 
		UltraDMD.ModifyScene00Ex id, toptext, bottomtext, duration
	ElseIf bUsePUPDMD Then
		If bPupStarted then pupDMDDisplay "default", toptext & "^" & bottomtext, "" ,Duration/1000, 0, 10
	End If 
End Sub


' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text. 3rd line is just 1 character
' Example format:
' DMD "text1","text2","backpicture", eNone, eNone, eNone, 250, True, "sound"
' Short names:
' dq = display queue
' de = display effect
' *************************************************************************

Const eNone = 0        ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3       ' Blink (blinks for 'TimeOn')
Const eBlinkFast = 4   ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)

Const dqSize = 64

Dim dqHead
Dim dqTail
Dim deSpeed
Dim deBlinkSlowRate
Dim deBlinkFastRate

Dim dCharsPerLine(2)
Dim dLine(2)
Dim deCount(2)
Dim deCountEnd(2)
Dim deBlinkCycle(2)

Dim dqText(2, 64)
Dim dqEffect(2, 64)
Dim dqTimeOn(64)
Dim dqbFlush(64)
Dim dqSound(64)

Sub DMD_Init() 'default/startup values
    Dim i, j

	If bUseUltraDMD then LoadUltraDMD()

    DMDFlush()
    deSpeed = 20
    deBlinkSlowRate = 5
    deBlinkFastRate = 2
    dCharsPerLine(0) = 26
    dCharsPerLine(1) = 26
    dCharsPerLine(2) = 3
    For i = 0 to 2
        dLine(i) = Space(dCharsPerLine(i))
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
    For i = 0 to 2
        For j = 0 to 64
            dqText(i, j) = ""
            dqEffect(i, j) = eNone
        Next
    Next
    DMD dLine(0), dLine(1), dLine(2), eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDFlush()
	DMDClearQueue
End Sub

Sub DMDScore()
	If bUseUltraDMD Then 
		If Not UltraDMD.IsRendering Then
			if tmrMedleyTour.Enabled  then 
				TimeStr = "Hits:" & 1234 & " Sw:" & 1234
			elseif tmrFinalTour.Enabled  then 
				TimeStr = "Hits:" & 12345
			elseif PlayerMode = -1 Then
				If bSecondMode then
					If PlayerMode2 = -1 then 
						TimeStr = ""
					Else 
						TimeStr = Mode2Percent(PlayerMode2) & "%  HITS:" & SwitchHitCount & " 2nd"
					End If 
				else 
					TimeStr = "Select Mode"
				End If 
			End If
			If Score(1) < 2300000000 then 		' UltraDMD cant handle alything greater than 2.4 billion ???? sux
				D  PlayersPlayingGame & "+" & CurrentPlayer & "+" & Score(0) & " + " & Score(1) ', Score(2), Score(3), "Ball " & CStr(Balls)
				UltraDMD.DisplayScoreboard PlayersPlayingGame, CurrentPlayer+1, Score(0), Score(1), Score(2), Score(3),"", "Ball " & CStr(Balls)
			End If 
		End If
	End If
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
	DisplayDMDText Text0, Text1, TimeOn
	PlaySoundVol Sound, VolDef
End Sub

Function ExpandLine(TempStr, id) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(dCharsPerLine(id))
    Else
        if(Len(TempStr) > Space(dCharsPerLine(id)))Then
            TempStr = Left(TempStr, Space(dCharsPerLine(id)))
        Else
            if(Len(TempStr) < dCharsPerLine(id))Then
                TempStr = TempStr & Space(dCharsPerLine(id)- Len(TempStr))
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString

    NumString = CStr(abs(Num))

	For i = Len(NumString)-3 to 1 step -3
		If IsNumeric(mid(NumString, i, 1))then
			NumString = left(NumString, i) & "," & right(NumString, Len(NumString)-i)
		end if
	Next

    FormatScore = NumString
End function

Function CL(id, NumString) 'center line
    Dim Temp, TempStr
	NumString = LEFT(NumString, dCharsPerLine(id))
    Temp = (dCharsPerLine(id)- Len(NumString)) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(id, NumString) 'right line
    Dim Temp, TempStr
	NumString = LEFT(NumString, dCharsPerLine(id))
    Temp = dCharsPerLine(id)- Len(NumString)
    TempStr = Space(Temp) & NumString
    RL = TempStr
End Function

Function FL(id, aString, bString) 'fill line
    Dim tmp, tmpStr
	aString = LEFT(aString, dCharsPerLine(id))
	bString = LEFT(bString, dCharsPerLine(id))
    tmp = dCharsPerLine(id)- Len(aString)- Len(bString)
	If tmp <0 Then tmp = 0
    tmpStr = aString & Space(tmp) & bString
    FL = tmpStr
End Function

Function LPad(StringToPad, Length, CharacterToPad)
  Dim x : x = 0
  If Length > Len(StringToPad) Then x = Length - len(StringToPad)
  LPad = String(x, CharacterToPad) & StringToPad
End Function

Function RPad(StringToPad, Length, CharacterToPad)
  Dim x : x = 0
  If Length > Len(StringToPad) Then x = Length - len(StringToPad)
  RPad = StringToPad & String(x, CharacterToPad)
End Function

'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
    RollingUpdate
End Sub

'********************************************************************************************
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first myVersion
    If TypeName(MyLight) = "Light" Then
        If FinalState = 2 Then
            FinalState = MyLight.State 'Keep the current light state
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then

        Dim steps

        ' Store all blink information
        steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
        If FinalState = 2 Then                      'Keep the current flasher state
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

        ' Start blink timer and create timer subroutine
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub


'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 11 colors: red, orange, amber, yellow...
'******************************************

Sub SetLightColor(n, col, stat)
	D2 "SetLightColor " & n.name & " " & col & " " & stat
    Select Case col
		Case "cyan"
			n.color = RGB(0, 128, 128)
			n.colorfull = RGB(0, 231, 231)
        Case "red"
            n.color = RGB(200, 0, 0)
            n.colorfull = RGB(255, 0, 0)
        Case "orange"
            n.color = RGB(18, 3, 0)
            n.colorfull = RGB(255, 64, 0)
        Case "yellow"
            n.color = RGB(200, 200, 0)
            n.colorfull = RGB(255, 255, 0)
        Case "green"
            n.color = RGB(0, 200, 0)
            n.colorfull = RGB(0, 255, 0)
		Case "green-intense"
            n.color = RGB(0, 255, 0)
            n.colorfull = RGB(0, 255, 0)
        Case "blue"
            n.color = RGB(0, 0, 200)
            n.colorfull = RGB(0, 0, 255)
        Case "blue-light"
            n.color = RGB(18, 18, 18)
            n.colorfull = RGB(18, 18, 255)
        Case "white"
            n.color = RGB(255, 252, 224)  ' value is used on trigers so dont change
            n.colorfull = RGB(193, 91, 0)
        Case "purple"
            n.color = RGB(128, 0, 128)
            n.colorfull = RGB(255, 0, 255)
        Case "amber"
            n.color = RGB(193, 49, 0)
            n.colorfull = RGB(255, 153, 0)        
		Case "pink"
            n.color = RGB(255,192, 203)
            n.colorfull = RGB(255,204, 255)
		Case Else
			msgbox  "Unknown color " & col
    End Select
    If stat <> -1 Then
        n.State = 0
		n.BlinkInterval = BlinkIntDef
        n.State = stat
    End If
End Sub

Sub SetLightColorRestore(light, lIndex)
	Dim finalColor
	Dim finalState 
	Dim i
	D2 "SetLightColorRestore " & light.name & " " & lIndex
	If lIndex = -1 then 					' Got get it If they didnt already have it 
		lIndex = GetModeIndex(light.name)
	End If 

	finalState = 0
	finalColor = "white"					' Should never need this line but just in Case 
	for i = 0 to StackSize-1				' Set the real color and state based on the stack  (higher in the stack is higher prioroty)
		If StackState(i).bStackActive then 
			If StackState(i).ArrowStates(lIndex).ArrowState <> 0 then 
				finalState = StackState(i).ArrowStates(lIndex).ArrowState
				finalColor = StackState(i).ArrowStates(lIndex).ArrowColor
				D2 "Stack: " & i & " lindex:" & lIndex & " state:" & finalState & " color:" & finalColor
			End If 
		End If  
	Next
	D2 "SetLightColorRestore Setting it to.." & light.name & " state:" & finalState & " color:" & finalColor
	SetLightColor light, finalColor, finalState
End Sub 

Sub SSetLightColor(stackIndex, light, color, state)		' StackSetLightColor		
	dim lIndex
	lIndex = GetModeIndex(light.name)
	If lIndex <> -1 then 
		StackState(stackIndex).ArrowStates(lIndex).ArrowColor = color
		StackState(stackIndex).ArrowStates(lIndex).ArrowState = state
		D2 "SSetLightColor " & stackIndex & " idx:" & lIndex & " Light:" & light.name & " state:" & state & " Color:" & color
		SetLightColorRestore light, lIndex
	else 
		D2 "Light Color Error:" & light.name & " idx:" & stackIndex & " " & lIndex
	End If 
End Sub

Sub SSetLightColorName(stackIndex, lightName, color, state)		' StackSetLightColor		
	dim lIndex, a
	D "SSetLightColorName "  & lightName & " idx:" & stackIndex & " color:" & color

	lIndex = GetModeIndex(lightName)
	If lIndex <> -1 then 
		StackState(stackIndex).ArrowStates(lIndex).ArrowColor = color
		StackState(stackIndex).ArrowStates(lIndex).ArrowState = state
		D "SSetLightColor " & stackIndex & " idx:" & lIndex & " Light:" & lightName & " state:" & state & " Color:" & color
		For Each a in aRampLights
			If UCase(a.name) = UCase(lightName) then 
				SetLightColorRestore a, lIndex
			End If
		Next

	else 
		D "Light Color Name Error:" & lightName & " idx:" & stackIndex & " " & lIndex
	End If 
End Sub

Sub flshCIU_Timer()
    If Lampz.State(2) then 
		Lampz.SetLamp 2, False
	Else
		Lampz.SetLamp 2, True
	End If 
End Sub

Sub setCIULight(bValue)
	If bValue then 
		flshCIU.TimerEnabled = True
		QueueScene "SceneMessage ""CRANK IT UP"",""IS LIT"","""" '", 1000, 2
		QueueScene "SceneClearMessage '", 0, 2
		PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F1cranklit.wav",100, 1
	Else 
		flshCIU.TimerEnabled = False
		Lampz.SetLamp 2, False
	End If 
End Sub 

Sub setModeSelectLight(bValue)
	If bValue Then
		SetLightColor I34, "white", 2
	Else
		SetLightColor I34, "white", 0
	End if
End Sub

Sub setExtraBallLight(bValue)
	If bValue Then
		SetLightColor I33, "orange", 2
	Else
		SetLightColor I33, "orange", 0
	End if
End Sub

Sub setMysteryLight(bValue)
	If bValue Then
		SetLightColor I38, "white", 2
	Else
		SetLightColor I38, "white", 0
	End if
End Sub


Sub flshDude_Timer()
    If Lampz.State(1) then 
		Lampz.SetLamp 1, False
	Else
		Lampz.SetLamp 1, True
	End If 
End Sub

Sub setDudeLight(bValue)
	If bValue then 
		flshDude.TimerEnabled = True
	Else 
		flshDude.TimerEnabled = False
		Lampz.SetLamp 1, False
	End If 
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Orbital Scoreboard Code
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

'****************************
' POST SCORES
'****************************
Dim osbtemp
Dim osbtempscore:osbtempscore = 0
If osbactive = 1 or osbactive = 2 Then osbtemp = osbdefinit
Sub SubmitOSBScore
	dim vers
	Dim ver
	vers=split(myVersion,".")			' Convert x.y.z to x.y
	ver=vers(0) & "." & vers(1)

	On Error Resume Next
	If osbactive = 1 or osbactive = 2 Then
		Dim objXmlHttpMain, Url, strJSONToSend 

		Url = "https://hook.integromat.com/82bu988v9grj31vxjklh2e4s6h97rnu0"
		strJSONToSend = "{""auth"":""" & osbkey &""",""player id"": """ & osbid & """,""player initials"": """ & osbtemp &""",""score"": " & CStr(osbtempscore) & ",""table"":"""& TableName & """,""version"":""" & ver & """}"

		Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
		objXmlHttpMain.open "PUT",Url, False
		objXmlHttpMain.setRequestHeader "Content-Type", "application/json"
		objXmlHttpMain.setRequestHeader "application", "application/json"

		objXmlHttpMain.send strJSONToSend
	end if
End Sub	

'****************************
' GET SCORES
'****************************
dim worldscores

Sub GetScores()
	If osbactive =0 then exit sub 
	If osbkey="" then exit sub
	On Error Resume Next
	Dim objXmlHttpMain, Url, strJSONToSend 

	Url = "https://hook.integromat.com/kj765ojs42ac3w4915elqj5b870jrm5c"

	strJSONToSend = "{""auth"":"""& osbkey &""", ""table"":"""& TableName & """}"

	Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
	objXmlHttpMain.open "PUT",Url, False
	objXmlHttpMain.setRequestHeader "Content-Type", "application/json"
	objXmlHttpMain.setRequestHeader "application", "application/json"

	objXmlHttpMain.send strJSONToSend

	worldscores = objXmlHttpMain.responseText
	vpmtimer.addtimer 3000, "showsuccess '"
	debug.print "Got OSB scores"
	'debug.print worldscores
	splitscores
End Sub	

Dim scorevar(22)
Dim dailyvar(22)
Dim weeklyvar(22)
Dim alltimevar(42)
Sub emptyscores
	dim i		
	For i = 0 to 42
		alltimevar(i) = "0"
	Next
	For i = 0 to 22
		weeklyvar(i) = "0"
		dailyvar(i) = "0"
	Next
End Sub

EmptyScores()


Sub splitscores
	On Error Resume Next
	dim a,scoreset,subset,subit,myNum,daily,weekly,alltime,x
	a = Split(worldscores,": {") 
	subset = Split(a(1),"[")

'		debug.print subset(1)
'		debug.print subset(2)
'		debug.print subset(3)
' daily scores
	myNum = 0
	daily = Split(subset(1),": ")
	for each x in daily
		myNum = MyNum + 1
		x = Replace(x, vbCr, "")
		x = Replace(x, vbLf, "")
		x = Replace(x, ",", "")
		x = Replace(x, """", "")
		x = Replace(x, "{", "")
		x = Replace(x, "}", "")
		x = Replace(x, "score", "")
		x = Replace(x, "initials", "")
		x = Replace(x, "weekly", "")
		x = Replace(x, "]", "")
		x = Replace(x, "alltime", "")
		dailyvar(MyNum) = x
		If dailyvar(MyNum) = "" Then
			If MyNum = 2 or 4 or 6 or 8 or 10 or 12 or 14 or 16 or 18 or 20 Then
				dailyvar(MyNum) = "OBS"
			Else
				dailyvar(MyNum) = 0
			end if
		end if
		debug.print "dailyvar(" &MyNum & ")=" & x
	Next

' weekly scores
	myNum = 0
	weekly = Split(subset(2),": ")
	for each x in weekly
		myNum = MyNum + 1
		x = Replace(x, vbCr, "")
		x = Replace(x, vbLf, "")
		x = Replace(x, ",", "")
		x = Replace(x, """", "")
		x = Replace(x, "{", "")
		x = Replace(x, "}", "")
		x = Replace(x, "score", "")
		x = Replace(x, "initials", "")
		x = Replace(x, "weekly", "")
		x = Replace(x, "]", "")
		x = Replace(x, "alltime", "")
		weeklyvar(MyNum) = x
		If weeklyvar(MyNum) = "" Then
			If MyNum = 2 or 4 or 6 or 8 or 10 or 12 or 14 or 16 or 18 or 20 Then
				weeklyvar(MyNum) = "OBS"
			Else
				weeklyvar(MyNum) = 0
			end if
		end if
		debug.print "weeklyvar(" &MyNum & ")=" & x
	Next

' alltime scores
	myNum = 0
	alltime = Split(subset(3),": ")
	for each x in alltime
		myNum = MyNum + 1
		x = Replace(x, vbCr, "")
		x = Replace(x, vbLf, "")
		x = Replace(x, ",", "")
		x = Replace(x, """", "")
		x = Replace(x, "{", "")
		x = Replace(x, "}", "")
		x = Replace(x, "score", "")
		x = Replace(x, "initials", "")
		x = Replace(x, "weekly", "")
		x = Replace(x, "]", "")
		x = Replace(x, "alltime", "")
		alltimevar(MyNum) = x
		If alltimevar(MyNum) = "" Then
			If MyNum = 2 or 4 or 6 or 8 or 10 or 12 or 14 or 16 or 18 or 20 or 22 or 24 or 26 or 28 or 30 or 32 or 34 or 36 or 38 or 40 Then
				alltimevar(MyNum) = "OBS"
			Else
				alltimevar(MyNum) = "0"
			end if
		end if
		debug.print "alltimevar(" &MyNum & ")=" & x
	Next

End Sub

sub showsuccess
	'pNote "Scoreboard","Updated"
	pupDMDDisplay "-","Scoreboard^Updated","",3,0,10
end sub

Class PinupNULL	' Dummy Pinup class so I dont have to keep adding If Cases when people dont choose pinup
	Public Sub LabelShowPage(screen, pagenum, vis, Special)
	End Sub
	Public Sub LabelSet(screen, label, text, vis, Special)
	End Sub
	Public Sub playlistplayex(screen, dir, fname, volume, priority)
	End Sub 
End Class 


'********************* START OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF BELOW   THIS LINE!!!! ***************
'******************************************************************************
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********
'******************************************************************************
'
'
'  Quick Steps:
'      1>  create a folder in PUPVideos with Starter_PuPPack.zip and call the folder "yourgame"
'      2>  above set global variable pGameName="yourgame"
'      3>  copy paste the settings section above to top of table script for user changes.
'      4>  on Table you need to create ONE timer only called pupDMDUpdate and set it to 250 ms enabled on startup.
'      5>  go to your tabe1_init or table first startup function and call PUPINIT function
'      6>  Go to bottom on framework here and setup game to call the appropriate events like pStartGame (call that in your game code where needed)...etc
'      7>  attractmodenext at bottom is setup for you already,  just go to each Case and add/remove as many as you want and setup the messages to show.  
'      8>  Have fun and use pDMDDisplay(xxxx)  sub all over where needed.  remember its best to make a bunch of mp4 with text animations... looks the best for sure!
'
'
'Note:  for *Future Pinball* "pupDMDupdate_Timer()" timer needs to be renamed to "pupDMDupdate_expired()"  and then all is good.
'       and for future pinball you need to add the follow lines near top
'Need to use BAM and have com idll enabled.
'				Dim icom : Set icom = xBAM.Get("icom") ' "icom" is name of "icom.dll" in BAM\Plugins dir
'				If icom is Nothing then MSGBOX "Error cannot run without icom.dll plugin"
'				Function CreateObject(className)       
'   					Set CreateObject = icom.CreateObject(className)   
'				End Function


Const HasPuP = True   'dont set to false as it will break pup

Const pTopper=0
Const pDMD=1
Dim pBackglass:pBackglass=5
Const pPlayfield=3
Const pMusic=4
Const pAudio=14
Const pCallouts=6
Const pBackglass2=7
Const pPup0=8
Const pBonusScreen = 12
Const pOverVid=13
Const pPopUP1=11
Const pPopUP2=12
Const pPopUP3=13
Const pPopUP4=14
Const pPopUP5=15
Const pPopUP6=16
Const pPopUP7=17
Const pPopUP8=18
Const pPopUP9=19

'pages
Const pDMDBlank=0
Const pScores=1
Const pBigLine=2
Const pThreeLines=3
Const pTwoLines=4
Const pLargerLetters=5

'dmdType
Const pDMDTypeLCD=0
Const pDMDTypeReal=1
Const pDMDTypeFULL=2


Dim PuPlayer
Dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pInAttract : pInAttract=false   'pAttract mode
Dim FSO, objFile
ModeNames(0)="LAST CHILD"
ModeNamesShort(0)="Last Child"
ModeNames(1)="WALK THIS WAY"
ModeNamesShort(1)="Walk This Way"
ModeNames(2)="SAME OLD SONG AND DANCE"
ModeNamesShort(2)="Same Old Song"
ModeNames(3)="SWEET EMOTION"
ModeNamesShort(3)="Sweet Emotions"
ModeNames(4)="DUDE LOOKS LIKE A LADY"
ModeNamesShort(4)="Dude Looks Like a Lady"
ModeNames(5)="BACK IN THE SADDLE"
ModeNamesShort(5)="Back In The Saddle"
ModeNames(6)="RATS IN THE CELLAR"
ModeNamesShort(6)="Rats"
ModeNames(7)="TOYS IN THE ATTIC"
ModeNamesShort(7)="Toys In The Attic"
ModeNames(8)="LOVE IN AN ELEVATOR"
ModeNamesShort(8)="Love"

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
    Randomize

	Redim LightsSaveColor(aLights.count, 4)
	Redim RampLightsSaveColor(aRampLights.count, 4)
	Redim saveColor(saveLights.count, 4)

	baLightsSaved=False
	baRampLightsSaved = False
	baSaveLightsSaved = True

	bTableReady=False
	bUseUltraDMD=False
	bUsePUPDMD=False
	bPupStarted=False
	If DMDMode = 1 then 
		bUseUltraDMD= True
		set PuPlayer = New PinupNULL
	elseIf DMDMode = 2 Then
		bUsePUPDMD = True
	Else 
		set PuPlayer = New PinupNULL
	End If 

	If b2son then 
		Controller.B2SSetData 1,1
		Controller.B2SSetData 2,1
		Controller.B2SSetData 3,1
		Controller.B2SSetData 4,1
		Controller.B2SSetData 5,1
		Controller.B2SSetData 6,1
		Controller.B2SSetData 7,1
		Controller.B2SSetData 8,1
	End If 

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 45 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFX("KickerKick", DOFContactors), SoundFX("Plunger_Release_No_Ball", DOFContactors)
        .CreateEvents "plungerIM"
    End With


    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    'load saved values, highscore, names, jackpot
    Loadhs

	'Set FSO = CreateObject("Scripting.FileSystemObject")

	'Set objFile = FSO.CreateTextFile("E:\aerosmith.txt",True)

    ' initalise the DMD display
    DMD_Init

    'Init main variables
	TableState_Init(0)
	TableState_Init(1)
	TableState_Init(2)
	TableState_Init(3)
	TableState_Init(4)

	for i = 0 to StackSize
		StackState_Init(i)
	Next

    ' freeplay or coins
    bFreePlay = False ' coins yes or no?

    ' initialse any other flags
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
	BallSaverActiveBuffer = 0
    bBallSaverReady = False
    bMultiBallMode = False
    bGameInPlay = False
	bCreatedBall = False
    bAutoPlunger = False
	bAutoPlunged = False
    bMusicOn = True
    BallsOnPlayfield = 0
	RealBallsInLock = 0
    CurrentPlayer = 0  ' needed to set early so Ultradmd doesnt crash
    PlayersPlayingGame = 0
    LastSwitchHit = ""
	SpellAerosmith = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bBonusHeld = False
    bJustStarted = True
    bInstantInfo = False

	DOF 142, DOFOff

	If bUsePUPDMD Then 
		PuPInit
	Else
		pupDMDUpdate.Enabled = False
	End if

	If bUseUltraDMD Then  ' Show an Intro Video on the DMD
		PlayDMDScene "Video-0x0098.wmv", "","", 9000
		vpmtimer.addtimer 4500, "bTableReady = True '"
	Else
		bTableReady = True
	End If
	bPupStarted=True

    EndOfGame()
End Sub

Sub Game_Init() 'called at the start of a new game
	Dim i,a
	bExtraBallWonThisBall = False

	pUpdateScores

	for each a in  flasherlights
		SetLightColor a, "white", -1
	Next
	SetLightColor BumperLight001, "white", 0:SetLightColor BumperLight002, "white", 0:SetLightColor BumperLight003, "white", 0

	MusicDir="Audioevents"
	VolMusic=0.7

	bAutoPlunger=False 
	bCreatedBall = False

	BallSearchCnt=0
	modesStarted = 0
	modesStarted = 0
	modesCompleted = 0
	SaveMode = -1

	bAutoPlunger=False
	SongNum = 1
	baLightsSaved = False
	baRampLightsSaved = False
	baSaveLightsSaved = False
	ballInLock=False
	ballInScoop=False
	RealBallsInLock=0

	' Reset game progress
	For i = 0 to 8
		ModeProgress(i) = 0
		ModePercent(i) = -1  ' 0 means you atleast attempted the mode
		Mode2Percent(i) = -1
		Mode2Progress(i) = 0
		Mode2Value(i) = 0
		Mode2Total(i) = 0
		ModeOrder(i) = -1
	Next

	modeRampColor = "white"
	bDebounce = False
	TurnOffPlayfieldLights()
	setCIULight(False)      'TODO testing
	setModeSelectLight(False)
	setExtraBallLight(False)
	setMysteryLight(False)
	setDudeLight(False)

	bMedleyTourCount=0
	bMedleyTourDone = False
	bMedleyTourReady = False

	For i = 0 to 8
		bMedleyTourProgress(i)=False
	Next

	bFinalTourDone=False
	bFinalTourReady=False
	bFinalTourCount=0

	SwitchHitCount = 0
	ModecountdownTimer.UserValue = 0
	ModecountdownTimer.Enabled = False
	SmartButtonCount = 0
	BumperMultiplier = 1

	ToyBoxMultiBallCount = 0
	ToyBoxMBJackpot = 0
	ToyBoxMBJackpotBase = 0   ' More per jackpot depending on how many balls locked
	ToyBoxMBJackpotTotal = 0
	ToyBoxMBJackpotHits = 0
	ToyBoxMBAttempts = 0
	ElevMultiBallCount = 0		

	For each a in aLockLights
		SetLightColor a, "green",0
		a.uservalue = 1   ' only 1 hit to make it solid
	Next

	'TESTING
	'SetLightColor I75, "green", 1 ' TESTING
	'i75.uservalue=0
	'SetLightColor I76, "green", 1 : RealBallsInLock=1
	'SetLightColor I77, "green", 2 : RealBallsInLock=2
	'SetLightColor I100, "green", 2

	SetLightColor F147, "white", 2

	ElevMBJackpot = 0
	ElevMBJackpotTotal = 0
	ElevMBJackpotHits = 0
	bElevMultiBall = False
	bShotMultiplierSelect = False
    MultiplierShot = 1
    Multiplier3x = 1
	bToyBoxBonus3x = False
	tmrToyBoxMBBonus.Enabled = False

	' Reset all the player states
	PlayerState(0).Reset		
	PlayerState(1).Reset
	PlayerState(2).Reset
	PlayerState(3).Reset
	PlayerState(4).Reset
	PlayerState(1).Save
	PlayerState(2).Save
	PlayerState(3).Save	
	PlayerState(4).Save

	bPauseTimer = False

	EnablePlayerSelect()  ' bad image
	PlayerMode = -1
	bSecondMode = False
	bBonusMode = False
	PlayerMode2 = -1
	bWizardMode = False
	MultiplierShot = 1

	Coins=0
	HiddenJesters=25
	SpinHits=25
	SpinValue=200
	SpinScore=0
	SpinScoreCurrent=0
	PopScore=0
	PopValue=0

	tmrShotMultiplierStrobe.Enabled = False

	DMDClearQueue		
	pDMDStartGame
End Sub


Sub EnablePlayerSelect()
	bPlayerModeSelect = True
	bRndModeShot = Int(7*Rnd())  ' Use this to lock in a mode shot for random shots
	pBGPlayerSelect
End sub


Sub pBGPlayerSelect 
	If bUsePUPDMD then 
		If modecountdowntimer.enabled Then
			pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1))+1)
		Else
			pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1)))
		End If
		PuPlayer.LabelSet pBackglass,"ModeProgress", " ",0,"{'mt':2,'color':111111,'width':12, 'height':23,'yalign':0,'ypos':4.0,'xpos':0,'pagenum':1}"
		PuPlayer.LabelSet pBackglass,"Time"," ",0,""
		PuPlayer.LabelSet pBackglass,"Aerosmith"," ",0,""
		PuPlayer.LabelShowPage pBackglass,1,0,""

		vpmtimer.addtimer 1500, "pBGPlayerInstruct '"
	End If
End Sub


Sub pBGPlayerInstruct
	If bUsePUPDMD then 
		PuPlayer.LabelSet pBackglass,"MessageT","USE FLIPPERS TO SELECT SONG2"	,1,"{'mt':1,'at':1,'fq':250}"
	End If
End Sub

Sub pBGGamePlay
	D "pBGGamePlay"
	If bUsePUPDMD then
		PuPlayer.LabelShowPage pBackglass,1,0,""
		PuPlayer.LabelSet pBackglass, "Aerosmith", "PuPOverlays\\aerosmith-"&"&SpellAerosmith"&".png",1,"{'mt':2,'color':111111,'width':31, 'height':18,'yalign':0,'ypos':0.0,'xpos':50.4}"
		If modecountdowntimer.enabled Then
			pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1))+1)
		Else
			pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1)))
		End If
		PuPlayer.LabelSet pBackglass,"MessageT"," "	,0,"{'mt':1,'at':1,'fq':250}"
	End If
End Sub


Sub pBgShowBonus(Message1, Message2)
	If bUsePUPDMD then
		If Message1 ="" and Message2 ="" Then
			PuPlayer.LabelSet pBonusScreen,"Message1", "" ,0,""
			PuPlayer.LabelSet pBonusScreen,"Message2", "" ,0,""
		Else
			D "pBGShowBonus: " & Message1 & " - " & Message2
			PuPlayer.LabelSet pBonusScreen,"Message1", Message1 ,1,""
			PuPlayer.LabelSet pBonusScreen,"Message2", Message2 ,1,""
		End If
	End If
End Sub


' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' If only one then decrement the remaining count AND test for End of game
' If more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()

	Dim tmpBallSaver
	Dim queueTime
	D "Drain_Hit Was BallsOnPlayfield: " & BallsOnPlayfield & " RealBallsInLock: " & RealBallsInLock

	tmpBallSaver=BallSaverActiveBuffer <> 0
	If BallSaverActiveBuffer>0 then BallSaverActiveBuffer = BallSaverActiveBuffer-1


	If (I14.state = 2) and bBallSaverActive=False then 
		D "ERROR Ball SAVE"
	End If 

    ' Destroy the ball
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1

    ' pretend to knock the ball into the ball storage mech
    RandomSoundDrain Drain
    'If Tilted the end Ball modes
    If Tilted Then
        StopEndOfBallModes
    End If
    ' If there is a game in progress AND it is not Tilted
    If(bGameInPlay = True)AND(Tilted = False)Then
        ' is the ball saver active (or did the switch catch us before the timer ran out)
        If bBallSaverActive or tmpBallSaver Then
            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in Case the multiballs are being ejected
			I "Ball Save multiball"
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
			DoBallSave
        Else
			D "Drain " & BallsOnPlayfield & " " & bMultiBallMode & " Real: " & RealBallsInLock

			If ToyBoxMB_End(True)	then exit Sub ' See If we need to throw more balls back into play

            ' cancel any multiball If on last ball (ie. lost all other balls)
            If((BallsOnPlayfield-RealBallsInLock = 1) and not bElevMultiBall and Not tmrMedleyTour.Enabled and Not tmrFinalTour.Enabled) or _
               (BallsOnPlayfield=1 and tmrMedleyTour.Enabled) or _
               (BallsOnPlayfield=1 and tmrFinalTour.Enabled) Then
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ChangeGi "white"
                    ' turn off any multiball specific lights                    
                    ToyBoxMB_End(False)
					EndWizardMode(1)
                End If
            End If

			If(BallsOnPlayfield-RealBallsInLock <= 1)Then	' Stop Elev Multiball
				If (bElevMultiBall) then 					' Reset Elevator Stuff
					bMultiBallMode = False					' Release last locked ball. Stop Multiball
					StopElevMultiball
				end if
			End If
			
            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then
				D "BallsOnPlayfield = 0 Condition"
                ' End Modes and timers
				ToyBoxMB_End(False)
				EndWizardMode(0)
				StopPlayerMode2
                StopEndOfBallModes
				If Mode2Percent(3)>0 then Mode2Percent(3)=100: D "SuperScoring is Ended***" ' Super Scoring Ends when ball ends
                ChangeGi "white"

				queueTime = getQueueTime					' See If we need to let animations finish playing 
				If queueTime = 0 then queueTime = 2500

                ' handle the end of ball (count bonus, change player, high score entry etc..)				
				If PlayerMode <> -1 Then			' Pause the countdown timer 
					ModeCountdownTimer.Enabled = False
				End If
                vpmtimer.addtimer queueTime, "EndOfBall '"
            End If
        End If
    End If
End Sub


Sub StopEndOfBallModes
	D "StopEndofBallModes"
	PauseBigScore=True
	puPlayer.LabelSet pBackglass,"BigScore"," ",0,""
	SceneClearPlayMessage
End Sub


Sub StopElevMultiball()
	Dim a
	I "StopElevMultiball : BallsonPF:" & BallsOnPlayfield & " RealLock:" & RealBallsInLock
'DEBUG	If RealBallsInLock<>0 then RealBallsInLock=RealBallsInLock-1		
	ShowPlayerModeComplete(1)	' Show Elevator MB Total
	bElevMultiBall=False
	SetLightColor F147, "white",2 'toybox
	for each a in aRampLights
		SSetLightColor kStack_Pri1, a, "pink", 0
	Next
	StackState(kStack_Pri1).Disable 
	PlaySong "Song-" & SaveMode & ".mp3"

	If ModeCountdownTimer.Enabled = False Then ' turn off the Petals and the TimerBubble
		PuPlayer.LabelSet pBackglass,"Time", " ",0,""
		pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1)))
		ShowPlayerModeComplete(-1)			' Show Mode total
	End If
	CheckWizardModesReady
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded
Dim tmpBonusTotal

Sub tmrEndOfBallBonus_Timer()
	dim i
	Dim XMultiplier
	dim FastBonus
	i=0
	FastBonus = 0
	tmrEndOfBallBonus.Interval = 1000
	tmrEndOfBallBonus.Enabled = False

	If tmrEndOfBallBonus.UserValue = 0 then 
		aRampLightsSave		' Save off the ramp colors 
	End If 

	If LFPress = 1 or RFPress = 1 then 		' Holding flippers speed up bonus 
		FastBonus = 500
		tmrEndOfBallBonus.Interval = 300
	End If

' spinners
' Targets
' bumpers
' loops
' ramps
' lanes
' 1x, 2x ,3x
' total bonus
 

	D "Start " & tmrEndOfBallBonus.UserValue
	Select Case INT(tmrEndOfBallBonus.UserValue)
		Case 0	'mpmpmp
			tmpBonusTotal = 0
			pBgShowBonus " "," "
			tmrEndOfBallBonus.UserValue = 1
			If PlayerMode <> -1 Then
				PuPlayer.LabelSet pBackglass,"Message1",ModeNames(PlayerMode),1,""
				PuPlayer.LabelSet pBackglass,"Message2",FormatScore(ModePoints),1,""
				PuPlayer.LabelSet pBackglass,"Message3","TOTAL",1,""
				DMD FormatScore(ModePoints), CL(1, ModeNamesShort(PlayerMode)), "", eBlink, eNone, eNone, 2000, False, ""
			Else
				tmrEndOfBallBonus.Interval = 100
			End If
		Case 1	' 
			If PlayerMode <> -1 Then
				PuPlayer.LabelSet pBackglass,"Message1"," ",0,""
				PuPlayer.LabelSet pBackglass,"Message2"," ",0,""
				PuPlayer.LabelSet pBackglass,"Message3"," ",0,""
				DMD "", "", "", eBlink, eNone, eNone, 2000, False, ""
			End If
			If SpinnerBonus = 0 Then
				tmrEndOfBallBonus.Interval = 100
			Else
				DMD CL(1, "SPINNER X " & SpinnerBonus), FormatScore(SpinnerBonus*2000), "", eBlink, eNone, eNone, 1000, False, ""
				pBgShowBonus "SPINNER X " & SpinnerBonus, FormatScore(SpinnerBonus*2000)	
				AddScore SpinnerBonus*2000
				tmpBonusTotal=tmpBonusTotal+(SpinnerBonus*2000)
			End If
			tmrEndOfBallBonus.UserValue = 2
		Case 2
			If TargetBonus = 0 Then
				tmrEndOfBallBonus.Interval = 100
			Else 
				DMD CL(1, "TARGETS X " & TargetBonus), FormatScore(TargetBonus*2000), "", eBlink, eNone, eNone, 1000, False, ""
				pBgShowBonus "TARGETS X " & TargetBonus, FormatScore(TargetBonus*2000)	
				AddScore TargetBonus*2000
				tmpBonusTotal=tmpBonusTotal+(TargetBonus*2000)
			End If
			tmrEndOfBallBonus.UserValue = 3
		Case 3
			If BumperBonus = 0 Then
				tmrEndOfBallBonus.Interval = 100
			Else
				DMD CL(1, "BUMPERS X " & BumperBonus), FormatScore(BumperBonus*2000), "", eBlink, eNone, eNone, 1000, False, ""
				pBgShowBonus "BUMPERS X " & BumperBonus, FormatScore(BumperBonus*2000)	
				AddScore BumperBonus*2000
				tmpBonusTotal=tmpBonusTotal+(BumperBonus*2000)
			End If
			tmrEndOfBallBonus.UserValue = 4
		Case 4 
			If LoopBonus = 0 Then
				tmrEndOfBallBonus.Interval = 100
			Else
				DMD CL(1, "LOOPS X " & LoopBonus), FormatScore(LoopBonus*2000), "", eBlink, eNone, eNone, 1000, False, ""
				pBgShowBonus "LOOPS X " & LoopBonus, FormatScore(LoopBonus*2000)	
				AddScore LoopBonus*2000
				tmpBonusTotal=tmpBonusTotal+(LoopBonus*2000)
			End If
			tmrEndOfBallBonus.UserValue = 5
		Case 5
			If RampBonus = 0 Then
				tmrEndOfBallBonus.Interval = 100
			Else
				DMD CL(1, "RAMPS X " & LoopBonus), FormatScore(RampBonus*5000), "", eBlink, eNone, eNone, 1000, False, ""
				pBgShowBonus "RAMPS X " & RampBonus, FormatScore(RampBonus*5000)	
				AddScore RampBonus*5000
				tmpBonusTotal=tmpBonusTotal+(RampBonus*5000)
			End If
			tmrEndOfBallBonus.UserValue = 6
		Case 6
			If LaneBonus = 0 Then
				tmrEndOfBallBonus.Interval = 100
			Else
				DMD CL(1, "LANES X " & LaneBonus), FormatScore(LaneBonus*5000), "", eBlink, eNone, eNone, 1000, False, ""
				pBgShowBonus "LANES X " & LaneBonus, FormatScore(LaneBonus*5000)	
				AddScore LaneBonus*5000
				tmpBonusTotal=tmpBonusTotal+LaneBonus*5000
			End If
			tmrEndOfBallBonus.UserValue = 7.1
		Case 7
			XMultiplier = round((tmrEndOfBallBonus.UserValue - 7) * 10)
			DMD CL(1, "TOTAL BONUS X " & XMultiplier), FormatScore(tmpBonusTotal * XMultiplier), "", eBlink, eNone, eNone, 1000, False, ""
			pBgShowBonus "TOTAL BONUS X " & XMultiplier, FormatScore(tmpBonusTotal * XMultiplier)	
			If BonusMultiplier > XMultiplier Then
				tmrEndOfBallBonus.UserValue=tmrEndOfBallBonus.UserValue+.1
				tmrEndOfBallBonus.Interval = 1000 - (XMultiplier*75)
				If tmrEndOfBallBonus.Interval < 300 then tmrEndOfBallBonus.Interval=300
			Else
				tmrEndOfBallBonus.UserValue = 8
			End If
		Case 8	' Show Total Bonus
			tmrEndOfBallBonus.UserValue = 9
			tmrEndOfBallBonus.Interval = 1000 - FastBonus
			DMD CL(1, "TOTAL BONUS"), FormatScore(tmpBonusTotal * BonusMultiplier), "", eBlink, eNone, eNone, 1000, False,  ""
			pBgShowBonus "TOTAL BONUS", FormatScore(tmpBonusTotal * BonusMultiplier)

			AddScore (tmpBonusTotal * BonusMultiplier)
			SetBonusMultiplier 1		' Push it back down to 1
		Case 9:
			If bUsePUPDMD then
				PuPlayer.LabelShowPage pBackglass,1,0,""
D "kDMD_PlayerMode:0"
				pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1)))
				pUpdateScores()
			End If
			tmrEndOfBallBonus.UserValue = 10
			tmrEndOfBallBonus.Interval = 2000 - FastBonus			
		Case 10
			pBgShowBonus "", ""
			pDMDEvent(kDMD_BonusClear)
			playclear pBonusScreen
			aRampLightsRestore		' Restore ramp colors

			pBGGamePlay			' Change the backglass to the play mode
			pDMDSetPage(pScores)
			WaitPlayerMode

			vpmtimer.addtimer 400, "EndOfBall2 '"
			pDMDEvent(kDMD_BonusClear)
			Exit Sub
	End Select 
	tmrEndOfBallBonus.Enabled = True
End Sub

Sub WaitPlayerMode()	' chooses the correct wait video 
	D "WaitPlayerMode playermode:" & PlayerMode
	'Dim VideoName 
	If PlayerMode <> -1 then
		If bUsePupDMD then 
'msgbox  "Try to remove the paused screen1"
			PuPlayer.playstop pOverVid
'msgbox  "Try to remove the paused screen2"
			PuPlayer.SetBackGround pOverVid, 0
'msgbox  "Try to remove the paused screen3"
			PuPlayer.SetLoop pOverVid, 0
'msgbox  "Try to remove the paused screen4"
			PuPlayer.playstop pOverVid
		End If 
'msgbox  "Try to remove the paused screen5"
		Select Case PlayerMode
			Case 0
				pDMDEvent(kDMD_last+10)
			Case 1
				pDMDEvent(KDMD_walk+10)
			Case 2
				pDMDEvent(kDMD_same+10)
			Case 3
				pDMDEvent(kDMD_sweet+10)
			Case 4
				pDMDEvent(kDMD_dude+10)
			Case 5
				pDMDEvent(kDMD_back+10)
			Case 6
				pDMDEvent(kDMD_rats+10)
		End Select
		'playmedia VideoName, "PupVideos", pOverVid, "", -1, "", 1, 1
'		If bUsePupDMD then PuPlayer.SetLoop pOverVid, 1
	End If 
End sub 


Sub EndOfBall()
    Dim AwardPoints, TotalBonus
	D "EndOfBall()"

    AwardPoints = 0
    TotalBonus = 0
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False
	If PlayerMode <> -1 Then			' Pause the countdown timer 
		ModeCountdownTimer.Enabled = False
		PuPlayer.LabelSet pBackglass,"Time", " ",0,""
	End If

	SceneClearMessage()

    ' only process any of this If the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

    If(Tilted = False) Then
        ' Count the bonus. This table uses several bonus
        DMDflush
		QueueFlush()

		If bUsePupDMD then PuPlayer.LabelShowPage pBonusScreen,1,0,"":PuPlayer.LabelShowPage pBackglass, 2,0,""
		
		pDMDEvent(kDMD_BonusBG)
		playmedia "Video-0x0000.mp4", "PupVideos", pBonusScreen, "", -1, "", 1, 1

        ' add a bit of a delay to allow for the bonus points to be shown & added up
		tmrEndOfBallBonus.Interval = 200
		tmrEndOfBallBonus.UserValue = 0		' Timer will start EndOfBall2 when it is done
		tmrEndOfBallBonus.Enabled = true
    Else
        vpmtimer.addtimer 100, "EndOfBall2 '"
    End If
End Sub


' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see If there are any extra balls for this player.
' If not, then check to see If this was the last ball (of the currentplayer)
'
Sub EndOfBall2()
	dim i
	dim thisMode
	D "EndOfBall2"
    ' If were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful If we are changing player LOL
    Tilted = False
    Tilt = 0
	PlayMultiplier = 1
    DisableTable False 'enable again bumpers and slingshots

	If PlayerMode = -1 Then	
		EnablePlayerSelect()
	End If


'	for i = 0 to 8	' Clear Bonus counters 
'		BonusArrowHits(i) = 0
'
'		' Completing the mode allows you to carryover the BonusModeTotal instead of blanking it
'		thisMode = Bonus2ModeIndex(i)
'		If thisMode <> -1 then 								' Make sure we got something valid 
'			If ModePercent(thisMode) < 100 then BonusModeTotal(i) =  0
'		End If 
'		'BonusModeTotal(i) = 0
'
'		BonusModeValue(i) = 0
'	next

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0) and bBallInPlungerLane=False Then	' Save Extra ball for later If there is a ball in the plunger lane
        D "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' If no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            I14.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD "_", CL(1, ("EXTRA BALL")), "_", eNone, eBlink, eNone, 1000, True, "vo_extraball"

'		If INT(RND * 2) = 0 then 
'			pDMDEvent(kDMD_ExtraBall)
'		Else
'			pDMDEvent(kDMD_ShootAgain)
'		End If 

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0) Then				' GAME OVER
            D "No More Balls, High Score Entry"
			If bUsePupDMD then 
				PuPlayer.PlayStop pOverVid						' Stop overlay If there is one
				PuPlayer.SetLoop pOverVid, 0
			End If
			playclear pAudio
			StopPlayerMode
			StopPlayerMode2

			' Turn off DOF so we dont accidently leave it on
			PlaySoundAt SoundFXDOF("Flipper_Left_Down_3", 101, DOFOff, DOFFlippers), LeftFlipper
			LeftFlipper.RotateToStart
			PlaySoundAt SoundFXDOF("Flipper_Right_Down_3", 102, DOFOff, DOFFlippers), RightFlipper
			RightFlipper.RotateToStart

            ' Submit the currentplayers score to the High Score system
            CheckHighScore()
			' you may wish to play some music at this point
        Else
            ' not the last ball (for that player)
            ' If multiple players are playing then move onto the next one
            EndOfBallComplete()
        End If
    End If
End Sub


' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'all of the same player)
'
Sub EndOfBallComplete()
    Dim NextPlayer
	dim Match

    D "EndOfBall - Complete"
    ' are there multiple players playing this game ?
    If(PlayersPlayingGame > 1)Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer > PlayersPlayingGame-1)Then
            NextPlayer = 0
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    D "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then
        ' set the machine into game over mode
		vpmtimer.addtimer getqueueTime+500, "EndOfGame() '"
    Else
        ' set the next player
		PlayerState(CurrentPlayer).bFirstBall = False
		PlayerState(CurrentPlayer).Save 
        CurrentPlayer = NextPlayer
		UpdateNumberPlayers				' Update the Score Sizes
        ' make sure the correct display is up to date
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

		PlayerState(CurrentPlayer).Restore

        ' AND create a new ball
        CreateNewBall()

        ' play a sound If more than 1 player
        If PlayersPlayingGame > 1 Then
            PlaySoundVol "say-player" &CurrentPlayer+1, VolDef
            DMD "", CL(1, "PLAYER " &CurrentPlayer+1), "", eNone, eNone, eNone, 800, True, ""
        End If
    End If
End Sub


' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    D "EndOfGame"	
	StopPlayerMode
	StopPlayerMode2
    bGameInPlay = False	
	tmrBallSearch.Enabled = False
    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0
	LFPress=0
	RFPress=0


    ' set any lights for the attract mode
    GiOff

	pDMDGameOver
End Sub


' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
	I "Creating the actual balls ..." & nBalls
    CreateMultiballTimer.Enabled = True
End Sub


' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait If there is a ball in the plunger lane
	CreateMultiballTimer.Interval = 2000
    If bBallInPlungerLane Then
        Exit Sub
    Else
        If BallsOnPlayfield < MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'If there are no more balls to eject then stop the timer
                Me.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            Me.Enabled = False
        End If
    End If
End Sub


Sub DoBallSave()
	D "DoBallSave"
	If bToyBoxMultiball or bElevMultiBall or tmrMedleyTour.Enabled or tmrFinalTour.Enabled then exit sub 	

	If bUseUltraDMD then 
		PlayDMDScene "vidBallSave.wmv", "", "", 4000
	Else
		pDMDEvent(582)
	End If
	DMD "_", CL(1, "BALL SAVED"), "_", eNone, eBlinkfast, eNone, 800, True, ""
End Sub 


' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see If a ball saver mechanism is needed and If so fire it up.


Sub swPlungerRest_Hit()
	Dim i
    D "swPlungerHit: ball in plunger lane: " & bPlayerModeSelect & " " & bAutoPlunger & " " & bCreatedBall
    ' some sound according to the ball position
    PlaySoundAt "Rubber_1", swPlungerRest
    bBallInPlungerLane = True
    ' kick the ball in play If the bAutoPlunger flag is on

	If bCreatedBall = False and AutoAI = False and AutoQA=False then	' If we didnt create a ball this must have gone up and back so kick it out automatically  
		bAutoPlunger = True
	End If 
	bCreatedBall = False
   
	If (bPlayerModeSelect) Then
		If PlayerMode = -1 then 
			If AutoAi or AutoQA Then ' force to selecct a random mode
				i=INT(10*RND())+2
				PlayerMode = (PlayerMode + i)
				If (PlayerMode > 6) Then PlayerMode=PlayerMode-6
				i=7 ' 8todo
				D "Random I mode is " & i
				DO
					i=i-1
					PlayerMode = (PlayerMode + 1) 
					If (PlayerMode > 6) Then PlayerMode=0
					D "Checking PlayerMode:" & PlayerMode & " %:" & ModePercent(PlayerMode)
					If i=0 then exit do
				Loop While ModePercent(PlayerMode) = 100
				If ModePercent(PlayerMode)=100 Then PlayerMode=-1
			Else
				SelectPlayerMode LeftFlipperKey ' force to select a mode
			End if
			D "Playermode Selected: " & PlayerMode
			if PlayerMode <> -1 Then UpdatePlayerMode()
		End If
		If PlayerState(CurrentPlayer).bFirstBall then 
			D "Start player callout"
			Select Case CurrentPlayer
				Case 0
					PuPlayer.playlistplayex pCallouts,"audioevents","player1-Sound-0x03E0.wav",100, 1
				Case 1
					PuPlayer.playlistplayex pCallouts,"audioevents","player2-Sound-0x03E1.wav",100, 1
				Case 2
					PuPlayer.playlistplayex pCallouts,"audioevents","player3-Sound-0x03E2.wav",100, 1
				Case 3
					PuPlayer.playlistplayex pCallouts,"audioevents","player4-Sound-0x03E3.wav",100, 1
			End Select
		End If 
	ElseIf PlayerMode <> -1 Then
		playclear pAudio  
		D "ResumeMedia and Video"
		resumemedia pBackglass
		resumemedia pMusic
	End If 

    If bAutoPlunger Then
        D "autofire the ball"
        PlungerIM.AutoFire
        DOF 125, DOFPulse
		DOF 112, DOFPulse
        bAutoPlunger = False
		bAutoPlunged = True
    End If

    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot
Sub swPlungerRest_UnHit()
	D "swPlungerRest_UnHit"
    bBallInPlungerLane = False
	bPauseTimer=False

	ResetBallSearch

'vpmtimer.addtimer 2000, "debug_same '"
If modeOrder(0)=-1 then vpmtimer.addtimer 300, "debug_medley '"

	If (bAutoPlunged = False) then		' Only start the skillshot If it is a manual plunge
		If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then ' todo and bWizardMode = False Then	' No skill shot on Wizard Modes 
			bSkillshotsReady(1) = True		' Enable the super skillshot
			StackState(kStack_Pri3).Enable(-1)
			SuperSkillShotIndex = 1  ' TODO INT(3 * RND)
			D "Skillshot " & SuperSkillShotIndex
			Select Case SuperSkillShotIndex
				Case 0
					SSetLightColor kStack_Pri3, I35, "white", 2
				Case 1
					SSetLightColor kStack_Pri3, I62, "white", 2
				Case 2
					SSetLightColor kStack_Pri3, I86, "white", 2
			End Select
		End If 
		tmrSkillShot.interval=8000:tmrSkillshot.Enabled = True
	End If
	bAutoPlunged =  False

	If (bFinalTourReady) then			' There are no modes to start
		PlayerMode = -1
	End If 

	' Check Mode
	If (bPlayerModeSelect) Then
		pBGGamePlay						' Change the backglass to the play mode
		aLightsSave						' Always start the skillshot
		StartPlayerMode()				' Start the mode and clear bPlayerModeSelect
	elseIf PlayerMode <> -1 then 		' If a mode is selected we should start the timer back Update
		vpmtimer.addtimer cInt(defBallSaverTime/4), "ModeCountdownTimer.Enabled = True '" ' delay the countdown
		' If we are paused resume media
		If bUsePupDMD then 
			PuPlayer.PlayStop pOverVid
			PuPlayer.SetLoop pOverVid, 0
		End If 
		RefreshPlayerMode
		playclear pAudio  ' turn off the callout
		'D "ResumeMedia and Video"
		'resumemedia pBackglass
		'resumemedia pMusic
	End If

	' If there is a need for a ball saver, then start off a timer
    ' only start If it is ready, and it is currently not running, else it will reset the time period
	D "Ballsaver Ready:" &  bBallSaverReady & " T:" & BallSaverTime & " Active:" & bBallSaverActive
    If(bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False)Then
        EnableBallSaver BallSaverTime
    End If

	I ">>LAUNCH BALL BOP: " & BallsOnPlayfield & " Real: " & RealBallsInLock
End Sub


Sub tmrSkillshot_Timer()
	bSkillshotsReady(0) = False		' toybox kicker
	bSkillshotsReady(1) = False		' Hold Left, drop down and hit CIU, Left Orb, Center Ramp
	D "Restore the Lights .. skillshot is over"
	StackState(kStack_Pri3).Disable
	Select Case SuperSkillShotIndex	' Turn it off 
		Case 0
			SetLightColorRestore I35, -1 'lsccop
		Case 1
			SetLightColorRestore I62, -1 'lorbit
		Case 2
			SetLightColorRestore I86, -1 'center ramp
	End Select
	tmrSkillshot.Enabled = False
End Sub


Sub EnableBallSaver(seconds)
    D "Ballsaver started " & seconds
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimerExpired.Interval = 1000 * seconds
    BallSaverTimerExpired.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' If you have a ball saver light you might want to turn it on at this point (or make it flash)
    I14.BlinkInterval = 160
    SetLightColor I14, "yellow", 2
End Sub


' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimerExpired_Timer()
	D2 "Ballsaver ended"
	BallSaverTimerExpired.Enabled = False
	vpmtimer.addtimer 1500, "bBallSaverActive = False '"    	' clear the flag after a slight buffer 
    ' If you have a ball saver light then turn it off at this point
    I14.State = 0
	BallSaverTime = defBallSaverTime
End Sub

Sub BallSaverTimerCancel()
	D "Ballsaver Cancel"
	BallSaverTimerExpired.Enabled = False
	bBallSaverActive = False
    ' If you have a ball saver light then turn it off at this point
    I14.State = 0
	BallSaverTime = defBallSaverTime	
End Sub 

Sub BallSaverSpeedUpTimer_Timer()
    D "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    I14.BlinkInterval = 80
    I14.State = 2
End Sub


Sub SetBackglassTimer(value)
	'D "SetBackglassTimer :" & kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1)) & " " & CurrentPlayer-1 & " " & PlayersPlayingGame*(PlayersPlayingGame-1)
	If bUsePUPDMD then
D "kDMD_PlayerMode:1"
		If modecountdowntimer.enabled Then
			pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1))+1)
			PuPlayer.LabelSet pBackglass,"Time", value		,1,""
		Else
			pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1)))
		End If
	End If 
End Sub


dim ModeCountdownTimerGrace
Sub ModeCountdownTimer_Timer()
	If bPauseTimer then exit Sub

	If ModeCountdownTimer.UserValue >0 then ModeCountdownTimer.UserValue = ModeCountdownTimer.UserValue -1
	SetBackglassTimer(ModeCountdownTimer.UserValue)

	If ModecountdownTimer.UserValue = 5 Then
		PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x030Btime.wav",100, 1
	End If
	If ModeCountdownTimer.UserValue <= 5 then 
		PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x01DB.wav",100, 1
	End If 
	If ModeCountdownTimer.UserValue = 1 then ModeCountdownTimerGrace=2	' setup 2 second grace period 
	If (ModeCountdownTimer.UserValue <= 0) Then
		ModeCountdownTimerGrace=ModeCountdownTimerGrace - 1
		If ModeCountdownTimerGrace <= 0 then 
			PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x030Ctimesup.wav",100, 1
'			If modecountdowntimer.enabled Then ' remove timer bubble       ' KEEP BUBBLE FOR THE secondMode
D "kDMD_PlayerMode:2"
			pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1))+1)
			StopPlayerMode()
		End If 
	End If
End Sub


'******
' Keys
'******
Sub tmrHoldKey_Timer()		' Reset the game with Ball in lane
	tmrHoldKey.Enabled = False
	' TBD Destroy balls
	
	If bBallInPlungerLane and BallsOnPlayfield = 1 Then
		PlaySoundVol "start", VolDef
		bResetCurrentGame=True
		If bUsePupDMD then 
			PuPlayer.PlayStop pOverVid						' Stop overlay If there is one
			PuPlayer.SetLoop pOverVid, 0
		End If
		ResetForNewGame()
	End If
End Sub


Sub Table1_KeyDown(ByVal Keycode)

	dim musicMode
	If bTableReady=False then Exit Sub	' If the ultraDMD hasnt finished intro then things look weird 
	If bBallInPlungerLane and keycode = StartGameKey then 
		tmrHoldKey.Enabled = False
		tmrHoldKey.Enabled = True
	End If

    If Keycode = AddCreditKey Then
		D "AddCreditKey"
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
		Credits = Credits + 1
		DOF 140, DOFOn
		If(Tilted = False)Then
			DMDFlush
			DMD "_", CL(1, "CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, "coin_In_1"
		End If
    End If
 
    If keycode = PlungerKey Then 
        Plunger.Pullback:SoundPlungerPull()
    End If

	If keycode = LeftFlipperKey Then lfPress = 1
	If keycode = RightFlipperKey Then rfPress = 1	

    'If keycode = 157 Then Nudge 90, 6:SoundNudgeLeft():CheckTilt
    'If keycode = 29  Then Nudge 270,6:SoundNudgeRight():CheckTilt

    If keycode = CenterTiltKey Then ' Nudge 0, 7:SoundNudgeCenter():CheckTilt
		'pDMDEvent(666)
		'ClearShotMultiplier(I101)
If Wall038.IsDropped then 
	Wall038.IsDropped=False 
    debugwall.isDropped=False
Else
	Wall038.IsDropped=True
	debugWall.IsDropped=True
End If
	'		QueueScene "SceneFinishMode 8 '", 6000, 1
	'		QueueScene "SceneClearFinishMode '", 0, 1

		'SuperLanes_Scene()
		'QueueScene "ScenePlayMessage ""ARS107-Scene-99.mp4"", """","""","""" '", 3000, 1
'PuPlayer.playlistplayex 15,"PupVideos","ARS107-Scene-99.mp4",0,1
		'QueueScene "SceneClearPlayMessage '", 0, 1

		'PuPlayer.playlistplayex 15,"PupVideos","ARS107-Scene-99.mp4",0,1
		'PuPlayer.playlistplayex pOverVid, "PupOverlays","ARS107-Scene-99.mp4", 8, 1   'testing transparency

'playmedia "ARS107-Scene-100.mp4", "PupVideos", pPopUp5, "", -1, "", 1, 1

'TransScenePlayMessage "","LINE1", "LINE2","LINE3"
		'D SpellAerosmith
		'SpellAerosmith=SpellAerosmith+1
		'PuPlayer.LabelSet pBackglass, "Aerosmith", "PuPOverlays\\aerosmith-"&SpellAerosmith&".png",1,"{'mt':2,'color':111111,'width':51, 'height':18,'yalign':0,'ypos':0.0,'xpos':50.4}"
	End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

	If bGameInPlay AND NOT Tilted Then
		' Normal flipper action
        If keycode = LeftFlipperKey Then SolLFlipper 1:StartInstantInfo(keycode)
        If keycode = RightFlipperKey Then SolRFlipper 1:StartInstantInfo(keycode)

		' Select Player Mode
		If (bPlayerModeSelect) and bInstantInfo=False Then	
			SelectPlayerMode(keycode)
		end If


		If keycode = RightMagnaSave or keycode = LockBarKey Then
			If bStartMB Then
				ToyBox_Cancel()
			End If
			CheckSmartButton True ' The routine will determine If it can be usedevent
		End if

		'  Start Mode
		If keycode = RightMagnaSave or keycode = LockBarKey or _  
			(keycode = PlungerKey and bUsePlungerForSternKey) Then

			If bPlayerModeSelect and bBallInPlungerLane = False then	' They selected a new player mode (kick the ball from the scoop)
				StartPlayerMode()
				vpmtimer.addtimer 2000, "ScoopKickOut '"
				exit sub
			elseIf bAutoPlunger=False and bBallInPlungerLane = True then	' Auto fire ball with stern key
				plungerIM.Strength = 60
				PlungerIM.AutoFire
				plungerIM.Strength = 45
			End if
		End if


        If keycode = StartGameKey Then		' startkey
            If((PlayersPlayingGame < MaxPlayers) AND (bOnTheFirstBall = True)) Then
                If(bFreePlay = True) Then
					soundStartButton()
                    PlayersPlayingGame = PlayersPlayingGame + 1
					If modecountdowntimer.enabled Then
						pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1))+1)
					Else
						pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1)))
					End If
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMDFlush
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "start"
                Else
                    If(Credits > 0)then
						soundStartButton()
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
						If Credits < 1 Then DOF 140, DOFOff
                        DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "start"
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush
                        DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, ""
                    End If
                End If
				UpdateNumberPlayers	' Update the screen layout on pup for multiple players			
            End If
        End If
    ElseIf bGameInPlay=False Then
		If keycode = StartGameKey Then
			If(bFreePlay = True)Then
				If(BallsOnPlayfield = 0)Then
					ResetForNewGame()
				End If
			Else
				If(Credits > 0)Then
					If(BallsOnPlayfield = 0)Then
						Credits = Credits - 1
						If Credits < 1 Then DOF 140, DOFOff
						ResetForNewGame()
					End If
				Else
					' Not Enough Credits to start a game.
					DMDFlush
					DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, ""
				End If
			End If
		End If 
    End If ' If (GameInPlay)
End Sub


Sub Table1_KeyUp(ByVal keycode)
	Dim NextCmd
	tmrHoldKey.Enabled = False
    If keycode = PlungerKey Then
        Plunger.Fire
		If bBallInPlungerLane then 
			SoundPlungerReleaseBall()
		Else	
			SoundPlungerReleaseNoBall()
		End If
    End If

    If hsbModeActive Then
		InstantInfoTimer.Enabled = False
		bInstantInfo = False
        Exit Sub
    End If

    If bGameInPlay AND NOT Tilted Then
		If LFPress=1 and RFPress = 1 then	' Pressed both at the same time
			If bFlipperSkipEnabled and FlipperSkipCmd<>"" then
				D "Skip Command >"&FlipperSkipCmd&"<"
				NextCmd = FlipperSkipCmd
				FlipperSkipCmd=""
				Execute NextCmd
			End If 
		End If 

		If keycode = LeftFlipperKey Then
			lfpress = 0
			leftflipper.eostorqueangle = EOSA
			leftflipper.eostorque = EOST
		End If
		If keycode = RightFlipperKey Then 
			rfpress = 0
			rightflipper.eostorqueangle = EOSA
			rightflipper.eostorque = EOST
		End If

        If keycode = LeftFlipperKey Then
			If bSkillshotsReady(1) = True Then bSkillshotsReady(1) = False  ' lose the super skill shot
            SolLFlipper 0
			EndFlipperStatus(keycode)
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
			EndFlipperStatus(keycode)
        End If
		'todo If keycode = LeftFlipperKey and tmrSkillshot.Enabled then tmrSkillShot.Enabled=False
	Else
		If keycode = LeftFlipperKey Then lfpress = 0
		If keycode = RightFlipperKey Then rfpress = 0
    End If
End Sub

'DWE
Sub UpdateNumberPlayers
	dim text1Size
	dim text2Size
	dim text3Size
	dim text4Size
	D "UpdateNumberPlayers"
	If bUsePUPDMD = False then Exit Sub 	' just PUP (for now)
	Select Case PlayersPlayingGame
		Case 1:
			text1Size=8*FontScale
			puPlayer.LabelSet pBackglass,"Play1score",FormatScore(score(0)),1,"{'mt':2,'size':  "&text1Size&", 'ypos': 100, 'xpos': 52, 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play2score","",0,""
			puPlayer.LabelSet pBackglass,"Play3score","",0,""
			puPlayer.LabelSet pBackglass,"Play4score","",0,""
		Case 2:
			If CurrentPlayer = 0 then text1Size=8*FontScale else text1Size=6*FontScale
			If CurrentPlayer = 1 then text2Size=8*FontScale else text2Size=6*FontScale
			puPlayer.LabelSet pBackglass,"Play1score",FormatScore(score(0)),1,"{'mt':2,'size':  "&text1Size&", 'ypos': 100, 'xpos': 30, 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play2score",FormatScore(score(1)),1,"{'mt':2,'size':  "&text2Size&", 'ypos': 100, 'xpos': 80, 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play3score","0",0,""
			puPlayer.LabelSet pBackglass,"Play4score","0",0,""
		Case 3:
			If CurrentPlayer = 0 then text1Size=6*FontScale else text1Size=5*FontScale
			If CurrentPlayer = 1 then text2Size=6*FontScale else text2Size=5*FontScale
			If CurrentPlayer = 2 then text3Size=6*FontScale else text3Size=5*FontScale
			puPlayer.LabelSet pBackglass,"Play1score",FormatScore(score(0)),1,"{'mt':2,'size':  "&text1Size&", 'ypos': 100, 'xpos': 25, 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play2score",FormatScore(score(1)),1,"{'mt':2,'size':  "&text2Size&", 'ypos': 100, 'xpos': 55, 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play3score",FormatScore(score(2)),1,"{'mt':2,'size':  "&text3Size&", 'ypos': 100, 'xpos': 85, 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play4score","0",0,""
		Case 4:
			If CurrentPlayer = 0 then text1Size=6*FontScale else text1Size=5*FontScale
			If CurrentPlayer = 1 then text2Size=6*FontScale else text2Size=5*FontScale
			If CurrentPlayer = 2 then text3Size=6*FontScale else text3Size=5*FontScale
			If CurrentPlayer = 3 then text4Size=6*FontScale else text4Size=5*FontScale
			puPlayer.LabelSet pBackglass,"Play1score",FormatScore(score(0)),1,"{'mt':2,'size':  "&text1Size&", 'ypos': 100, 'xpos': 10, 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play2score",FormatScore(score(1)),1,"{'mt':2,'size':  "&text2Size&", 'ypos': 100, 'xpos': 35, 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play3score",FormatScore(score(2)),1,"{'mt':2,'size':  "&text3Size&", 'ypos': 100, 'xpos': 60, 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play4score",FormatScore(score(3)),1,"{'mt':2,'size':  "&text4Size&", 'ypos': 100, 'xpos': 95, 'xalign': 1}"
	End Select
End Sub 


Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a  
'
Sub ResetForNewGame()
    Dim i
	D "ResetForNewGame"
    bGameInPLay = True
	TextBox.Text="ResetForNewGame"
	tmrBallSearch.Interval = kBallSearchTimeout
	BallSearchCnt = 0
	tmrBallSearch.Enabled = True

    'resets the score display, and turn off attrack mode
    StopAttractMode
	pBgShowBonus " ", " "
    GiOn
	SmartButtonFlash "", False

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 0
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
	PlayMultiplier = 1
    For i = 0 To MaxPlayers-1
        Score(i) = 0
        BonusPoints(i) = 0
        BonusHeldPoints(i) = 0
        BonusMultiplier = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
		TiltCount(i) = 0
    Next

    ' initialise any other flags
    Tilt = 0

    ' initialise Game variables
    Game_Init()

	UpdateNumberPlayers		' Do the first one 
    ' set up the start delay to handle any Start of Game Attract Sequence
    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
	If bResetCurrentGame = False then 
		CreateNewBall()
	End If
	bResetCurrentGame = False
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (If multiple are playing))

Sub ResetForNewPlayerBall()
	D "ResetForNewPlayerBall BALL " & Balls
    ' make sure the correct display is upto date
    AddScore 0
	mHMagnet.MagnetOn = 0

	SetBonusMultiplier 1
    ' set the current players bonus multiplier back down to 1X
    MultiplierShot = 1
	bShotMultiplierSelect = False
'	bWizardMode = False

	' Clear shot multipliers 
	I32.UserValue =0
	I67.UserValue =0
	I84.UserValue =0
	I95.UserValue =0
	I46.UserValue =0
	I112.UserValue=0
	I101.UserValue=0
	I89.UserValue =0

	coins=0

    ' reset any drop targets, lights, game modes etc..

    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False
    'Reset any table specific
    ResetNewBallVariables()
	ResetNewBallLights()
    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True
	PopValue = 5000
	SpinValue=200

	SpinnerBonus=0
	TargetBonus=0
	BumperBonus=0
	LoopBonus=0
	RampBonus=0
	LaneBonus=0

	D "Reset Skillshot"
	bSkillshotsReady(0) = True		' tbox kicker
	bSkillshotsReady(1) = False		' Hold Left, Loop around and hit Orb, Right Ramp or Right Orbit
	SuperSkillShotIndex=-1
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
	bCreatedBall = True
    BallRelease.CreateSizedball BallSize / 2

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    BallRelease.Kick 90, 4
    SoundSaucerKick 1, BallRelease

' If there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield-RealBallsInLock > 1 Then
		D "SETTING MULTIBALL MODE " & BallsOnPlayfield & " Real:" & RealBallsInLock & "!!!!!!!!!!!!!!!!!!"
        bMultiBallMode = True
        bAutoPlunger = True
    End If
End Sub


Sub ResetBallSearch()
	If BallSearchResetting then Exit Sub	' We are resetting just exit for now 
	'D "ResetBallSearch()"
	tmrBallSearch.Enabled = False	' Reset Ball Search
	BallSearchCnt=0
	tmrBallSearch.Enabled = True
End Sub 

Dim BallSearchResetting:BallSearchResetting=False

Sub tmrBallSearch_Timer()	' We timed out
	' See If we are in mode select, a flipper is up that might be holding the ball or a ball is in the lane 
	D "tmrBallSearchTimer()"
	If bGameInPlay and bPlayerModeSelect = False and _
		hsbModeActive = False and _ 
		tmrEndOfBallBonus.Enabled = False and _
		bBallInPlungerLane = False and _
		LeftFlipper.CurrentAngle <> LeftFlipper.EndAngle and _
		RightFlipper.CurrentAngle <> RightFlipper.EndAngle Then

		D "Ball Search - NO ACTIVITY " & BallSearchCnt

		If BallSearchCnt >= 3 Then
			dim Ball
			D "--- listing balls ---"
			For each Ball in GetBalls
				I "Ball: (" & Ball.x & "," & Ball.y & ")"
			Next
			D "--- listing balls ---"

			BallSearchCnt = 0
			If BallsOnPlayfield > 0 then 	' somehow we might have drained and didnt catch it??
				BallsOnPlayfield = BallsOnPlayfield - 1  ' We cant find the ball (remove one)
			End if
			I "Ball search .. add a ball"
			AddMultiball(1)
			DisplayDMDText "BALL SEARCH FAIL","", 1000
			Exit sub
		End if

		BallSearchResetting=True
		BallSearchCnt = BallSearchCnt + 1
		If BallSearchCnt > 2 Then
			DisplayDMDText "BALL SEARCH","", 1000
			DOF 113, DOFPulse
			DOF 112, DOFPulse
			mHMagnet.MagnetOn = False
		End If
		vpmtimer.addtimer 3000, "BallSearchResetting = False '"
	Else 
		ResetBallSearch()
	End If 
End Sub

'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit
	on error resume next
	Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
	PuPlayer.B2SInit "", pGameName
	on error goto 0
	If not IsObject(PuPlayer) then bUsePUPDMD=False


	If (PuPDMDDriverType=pDMDTypeReal) and (useRealDMDScale=1) Then 
       PuPlayer.setScreenEx pDMD,0,0,128,32,0  'If hardware set the dmd to 128,32
	End if

	PuPlayer.LabelInit pBackglass
	PuPlayer.LabelInit pDMD

	If PuPDMDDriverType=pDMDTypeReal then
		Set PUPDMDObject = CreateObject("PUPDMDControl.DMD") 
		PUPDMDObject.DMDOpen
		PUPDMDObject.DMDPuPMirror
		PUPDMDObject.DMDPuPTextMirror
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":33 }"             'set pupdmd for mirror and hide behind other pups
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":32, ""FQ"":3 }"   'set no antialias on font render If real
	Else
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":33 }"             'set pupdmd for mirror and hide behind other pups
	
	End If

	pSetPageLayouts
	pDMDSetPage(pDMDBlank)   'set blank text overlay page.
	pDMDStartUP				 ' firsttime running for a startup video..
End Sub 'end PUPINIT


'PinUP Player DMD Helper Functions

Sub pDMDLabelHide(labName)
	PuPlayer.LabelSet pDMD,labName,"",0,""   
End sub


Sub pDMDScrollBig(msgText,timeSec,mColor)
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
End sub


Sub pDMDScrollBigV(msgText,timeSec,mColor)
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
End sub


Sub pDMDSplashScore(msgText,timeSec,mColor)
	PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
End Sub


Sub pDMDSplashScoreScroll(msgText,timeSec,mColor)
	PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':"& (timeSec*1000) &", 'mlen':"& (timeSec*1000) &",'tt':0, 'fc':" & mColor & "}"
End Sub


Sub pDMDZoomBig(msgText,timeSec,mColor)  'new Zoom
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':3,'hstart':5,'hend':80,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
End sub


Sub pDMDTargetLettersInfo(msgText,msgInfo, timeSec)  'msgInfo = '0211'  0= layer 1, 1=layer 2, 2=top layer3.
'this function is when you want to hilite spelled words.  Like B O N U S but have O S hilited as already hit markers... see example.
	PuPlayer.LabelShowPage pDMD,5,timeSec,""  'show page 5
	Dim backText
	Dim middleText
	Dim flashText
	Dim curChar
	Dim i
	Dim offchars:offchars=0
	Dim spaces:spaces=" "  'set this to 1 or more depends on font space width.  only works with certain fonts
                          'If using a fixed font width then set spaces to just one space.

	For i=1 To Len(msgInfo)
		curChar="" & Mid(msgInfo,i,1)
		If curChar="0" Then
            backText=backText & Mid(msgText,i,1)
            middleText=middleText & spaces
            flashText=flashText & spaces          
            offchars=offchars+1
		End If
		If curChar="1" Then
            backText=backText & spaces
            middleText=middleText & Mid(msgText,i,1)
            flashText=flashText & spaces
		End If
		If curChar="2" Then
            backText=backText & spaces
            middleText=middleText & spaces
            flashText=flashText & Mid(msgText,i,1)
		End If   
	Next 

	If offchars=0 Then 'all litup!... flash entire string
		backText=""
		middleText=""
		FlashText=msgText
	End If  

	PuPlayer.LabelSet pDMD,"Back5"  ,backText  ,1,""
	PuPlayer.LabelSet pDMD,"Middle5",middleText,1,""
	PuPlayer.LabelSet pDMD,"Flash5" ,flashText ,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & "}"   
End Sub


Sub pDMDSetPage(pagenum)
	If (bUsePUPDMD) then
		D "Changing Page: " & pagenum
		PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page If want off
	End if
End Sub


Sub pHideOverlayText(pDisp)
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& pDisp &", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
End Sub


Sub pDMDShowLines3(msgText,msgText2,msgText3,timeSec)
	Dim vis:vis=1
	If pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,3,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash3a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash3b",msgText2,vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Splash3c",msgText3,vis,pLine3Ani
End Sub


Sub pDMDShowLines2(msgText,msgText2,timeSec)
	Dim vis:vis=1
	If pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,4,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash4a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash4b",msgText2,vis,pLine2Ani
End Sub

Sub pDMDShowCounter(msgText,msgText2,msgText3,timeSec)
	Dim vis:vis=1
	If pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,6,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash6a",msgText,vis, pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash6b",msgText2,vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Splash6c",msgText3,vis,pLine3Ani
End Sub


Sub pDMDShowBig(msgText,timeSec, mColor)
	Dim vis:vis=1
	If pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,vis,pLine1Ani
End sub


Sub pDMDShowHS(msgText,msgText2,msgText3,timeSec) 'High Score
	Dim vis:vis=1
	If pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,7,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash7a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash7b",msgText2,vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Splash7c",msgText3,vis,pLine3Ani
End Sub


Sub pDMDSetBackFrame(fname)
	PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
End Sub

Sub pDMDStartBackLoop(fPlayList,fname)
	PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
	PuPlayer.SetBackGround pDMD,1
End Sub

Sub pStartBackLoop(fPlayList,fname)
	D "pStartBackLoop " & fPlayList & " " & fname	
	If bUsePupDMD then ' 01d
		PuPlayer.playlistplayex pBackglass,fPlayList,fname,100,1
		PuPlayer.SetBackGround pBackglass,1
	End IF
end Sub

Sub pDMDStopBackLoop
	If bUsePupDMD then ' 01d
		PuPlayer.SetBackGround pDMD,0
		PuPlayer.playstop pDMD
	End If
End Sub


Dim pNumLines

'Theme Colors for Text (not used currenlty,  use the |<colornum> in text labels for colouring.
Dim SpecialInfo
Dim pLine1Color : pLine1Color=8454143  
Dim pLine2Color : pLine2Color=8454143
Dim pLine3Color :  pLine3Color=8454143
Dim curLine1Color: curLine1Color=pLine1Color  'can change later
Dim curLine2Color: curLine2Color=pLine2Color  'can change later
Dim curLine3Color: curLine3Color=pLine3Color  'can change later


Dim pDMDCurPriority: pDMDCurPriority =-1
Dim pDMDDefVolume: pDMDDefVolume = 0   'default no audio on pDMD

Dim pLine1
Dim pLine2
Dim pLine3
Dim pLine1Ani
Dim pLine2Ani
Dim pLine3Ani

Dim PriorityReset:PriorityReset=-1
DIM pAttractReset:pAttractReset=-1
DIM pAttractBetween: pAttractBetween=2000 '1 second between calls to next attract page
DIM pDMDVideoPlaying: pDMDVideoPlaying=false


'************************ where all the MAGIC goes,  pretty much call this everywhere  ****************************************
'****************************************   DONT TOUCH THIS CODE   ************************************************************

Sub pupDMDDisplay(pEventID, pText, VideoName, TimeSec, pAni, pPriority)
' pEventID = reference If application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' 			also global variable useDMDVideos=true/false If user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation If any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345
	DIM curPos
	D "pupDMDDisplay " & pEventID & " " & pText & " " & VideoName

	If pDMDCurPriority>pPriority then 
		Exit Sub  'If something is being displayed that we don't want interrupted.  same level will interrupt.
		D "DMD Skipping - Hi Pri"
	End If 
	pDMDCurPriority=pPriority
	If timeSec=0 then timeSec=1 'don't allow page default page by accident

	pLine1=""
	pLine2=""
	pLine3=""
	pLine1Ani=""
	pLine2Ani=""
	pLine3Ani=""

	If pAni=1 Then  'we flashy now aren't we
		pLine1Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
		pLine2Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
		pLine3Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
	End If

	curPos=InStr(pText,"^")   'Lets break apart the string If needed
	If curPos>0 Then 
	   pLine1=Left(pText,curPos-1) 
	   pText=Right(pText,Len(pText) - curPos)
	   
	   curPos=InStr(pText,"^")   'Lets break apart the string
	   If curPOS>0 Then
		  pLine2=Left(pText,curPos-1) 
		  pText=Right(pText,Len(pText) - curPos)

		  curPos=InStr("^",pText)   'Lets break apart the string   
		  If curPos>0 Then
			 pline3=Left(pText,curPos-1) 
		  Else 
			If pText<>"" Then pline3=pText 
		  End If 
	   Else 
		  If pText<>"" Then pLine2=pText
	   End If    
	Else 
	  pLine1=pText  'just one line with no break 
	End if


	'lets see how many lines to Show
	pNumLines=0
	If pLine1<>"" then pNumLines=pNumlines+1
	If pLine2<>"" then pNumLines=pNumlines+1
	If pLine3<>"" then pNumLines=pNumlines+1

	If pDMDVideoPlaying Then 
		PuPlayer.playstop pDMD
		pDMDVideoPlaying=False
	End if


	If (VideoName<>"") and (useDMDVideos) Then  'we are showing a splash video instead of the text.	
		PuPlayer.playlistplayex pDMD,"DMDSplash",VideoName,pDMDDefVolume,pPriority  'should be an attract background (no text is displayed)
		pDMDVideoPlaying=true
	End If 'If showing a splash video with no text


	If StrComp(pEventID,"shownum",1)=0 Then              'check eventIDs
		pDMDShowCounter pLine1,pLine2,pLine3,timeSec
	ElseIf StrComp(pEventID,"target",1)=0 Then              'check eventIDs
		pDMDTargetLettersInfo pLine1,pLine2,timeSec
	ElseIf StrComp(pEventID,"highscore",1)=0 Then              'check eventIDs
		pDMDShowHS pLine1,pLine2,pline3,timeSec
	ElseIf (pNumLines=3) Then                'depends on # of lines which one to use.  pAni=1 will flash.
		pDMDShowLines3 pLine1,pLine2,pLine3,TimeSec
	ElseIf (pNumLines=2) Then
		pDMDShowLines2 pLine1,pLine2,TimeSec
	ElseIf (pNumLines=1) Then
		pDMDShowBig pLine1,timeSec, curLine1Color
	Else
		pDMDShowBig pLine1,timeSec, curLine1Color
	End if

	PriorityReset=TimeSec*1000
End Sub 'pupDMDDisplay message

Sub pupDMDupdate_Timer()
	'D "pupDMDupdate_Timer()"
	pUpdateScores

    If PriorityReset>0 Then  'for splashes we need to reset current prioirty on timer
		PriorityReset=PriorityReset-pupDMDUpdate.interval
		If PriorityReset<=0 Then 
            pDMDCurPriority=-1            
            If pInAttract then pAttractReset=pAttractBetween ' pAttractNext  call attract next after 1 second
			pDMDVideoPlaying=false	
		End if
    End if

    If pAttractReset>0 Then  'for splashes we need to reset current prioirty on timer
		pAttractReset=pAttractReset-pupDMDUpdate.interval
		If pAttractReset<=0 Then 
            pAttractReset=-1            
            If pInAttract then pAttractNext
		End if
    End If 
End Sub

Sub PuPEvent(EventNum)
	If hasPUP=false then Exit Sub
	PuPlayer.B2SData "E"&EventNum,1  'sEnd event to puppack driver  
End Sub


'********************* END OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF ABOVE THIS LINE!!!! ***************
'****************************************************************************

'*****************************
' AUTO TESTING
' by:NailBuster 
' Global variable "AutoQA" below will switch all this on/off during testing.  
'
'*****************************
' NailBusters AutoQA Code and triggers..  
' add a timer called AutoQAStartGame.  you can run every 10000 interval.


Dim QACoinStartSec:QACoinStartSec=3 'TDOD was 30   'timeout seconds for AutoCoinStartSec
Dim QANumberOfCoins:QANumberOfCoins=1 'number of coins to add for each start
Dim QASecondsDiff

Dim QALastFlipperTime:QALastFlipperTime=Now()
Dim AutoFlipperLeft:AutoFlipperLeft=false
Dim AutoFlipperRight:AutoFlipperRight=false
If AutoQa or AutoAI then TriggerLeftAuto.Enabled = True
If AutoQa or AutoAI then TriggerRightAuto.Enabled = True
'If AutoQa or AutoAI then 
'	AutoQAStartGame.interval=5000
'	AutoQAStartGame.Enabled = True		' Start it up If this is enabled 
'end if

Sub AutoQAStartGame_Timer()                 'this is a timeout when sitting in attract with no flipper presses for 60 seconds, then add coins and start game.
 If AutoQA=false and AutoAI=False Then Exit Sub
 AutoQAStartGame.Enabled = False

 QASecondsDiff = DateDiff("s",QALastFlipperTime,NOW())
 If QASecondsDiff>QACoinStartSec Then
	D "AddSomeCoins"
    'simulate quarters and start game keys
    Dim fx : fx=0
    Dim keydelay : keydelay=100
	Do While fx<QANumberOfCoins  
		vpmtimer.addtimer keydelay,"Table1_KeyDown(AddCreditKey) '"
        vpmtimer.addtimer keydelay+200,"Table1_KeyUp(AddCreditKey) '"
        keydelay=keydelay+500
		fx=fx+1
	Loop
    vpmtimer.addtimer keydelay,"Table1_KeyDown(StartGameKey) '"
    vpmtimer.addtimer keydelay+200,"Table1_KeyUp(StartGameKey) '"
    QALastFlipperTime=Now() 
	AutoFlipperLeft=false 	
    AutoFlipperRight=false
 End If  
  If QASecondsDiff>30 Then   'safety of stuck up flippers.
   AutoFlipperLeft=false 	
   AutoFlipperRight=false
  End if
  If bGameInPlay and QASecondsDiff>20 Then   'Press the magna
	vpmtimer.addtimer keydelay,"Table1_KeyDown(RightMagnaSave) '"
	vpmtimer.addtimer keydelay+200,"Table1_KeyUp(RightMagnaSave) '"
  End if
End Sub


Sub TriggerAutoPlunger_Hit()          'add a trigger in front of plunger.  adjust the delay timings If needed.    
    If AutoQA=false and AutoAI=false Then Exit Sub
	vpmtimer.addtimer 300,"Table1_KeyDown(PlungerKey) '"
    vpmtimer.addtimer int(1200+(0*RND())),"Table1_KeyUp(PlungerKey) '"   ' 8todo
End Sub


Sub FlipperUP(which)  'which=1 left 2 right
	QALastFlipperTime=Now()
	If which=1 Then
		Table1_KeyDown(LeftFlipperKey)   
		vpmtimer.addtimer 200+int(200*Rnd()),"Table1_KeyUP(LeftFlipperKey):AutoFlipperLeft=false  '"    
	Else
		Table1_KeyDown(RightFlipperKey)
		vpmtimer.addtimer 200+int(200*Rnd()),"Table1_KeyUP(RightFlipperKey):AutoFlipperRight=false  '"    
	End If
End Sub

Sub TriggerLeftAuto_Hit()
	If AutoQA And AutoFlipperLeft=false then vpmtimer.addtimer 20+int(20*Rnd()),"FlipperUP(1) '" 
    AutoFlipperLeft=true
End Sub

Sub TriggerRightAuto_Hit()
	If AutoQA and AutoFlipperRight=false then vpmtimer.addtimer 20+int(20*Rnd()),"FlipperUP(2) '" 
    AutoFlipperRight=true
End Sub
'********************************** AI
Sub TurnOnAI
	TriggerLF001.enabled=1:TriggerRF001.enabled=1
	ZoneLeft.enabled=1:ZoneRight.enabled=1
	ZoneLeft1.enabled=1:ZoneRight1.enabled=1
	LeftFlipper.scatter=0
	RightFlipper.scatter=0
End Sub

Sub TurnOffAI
	TriggerLF001.enabled=0:TriggerRF001.enabled=0
	ZoneLeft.enabled=0:ZoneRight.enabled=0
	ZoneLeft1.enabled=0:ZoneRight1.enabled=0
End Sub

Sub  ZoneLeft_Hit
'	D "ZoneLeft_Hit() - check"
    If BallsOnPlayfield = 1 Then  
       If Activeball.velX<4 and Activeball.velX>-4 Then
		     If Activeball.velY<16.5 and Activeball.velY>-16.5 Then 
				D "ZoneLeft_Hit() - press flipper"
				Table1_KeyDown(LeftFlipperKey)
				CradleLeft.Enabled=1
				TriggerLF001.enabled=0
		    End If
	     End If
    End if
End Sub

Sub ZoneLeft_Unhit
'	D "ZoneLeft_unhit - release flipper"
	Table1_KeyUp(LeftFlipperKey)
	CradleLeft.Enabled=0:CradleLeftTimer.Enabled=0
	TriggerLF001.enabled=1
End Sub

Sub CradleLeft_Hit
D "CradleLeft"
	CradleLeftTimer.interval=750:CradleLeftTimer.Enabled=1
	If BallsOnPlayfield=1 Then bPauseTimer=True
End Sub

Sub CradleLeft_UnHit
	bPauseTimer=False
	CradleLeftTimer.Enabled=0
End Sub

Sub CradleLeftTimer_Timer
'	D "CradleLeftTimer_ release flipper"
	CradleLeftTimer.Enabled=0
	Table1_KeyUp(LeftFlipperKey)
	TriggerLF001.enabled=0
	CheckShotPriority 0
End Sub


Sub ZoneLeft1_Hit
	If BouncePassRTimer.Enabled = True Then Exit Sub
    If Activeball.velY>13.7 and Activeball.velY<18 and BallsOnPlayfield = 1 Then
		D2 "ZoneLeft1_Hit() vel=" & CStr(Activeball.velY) & "velX=" & CStr(Activeball.velX)
		If Activeball.velX<-2 Then
			D "Bounce Pass"
			TriggerLF001.Enabled=0
			TriggerRF001.Enabled=0
			ZoneLeft.Enabled=0
			ZoneRight.Enabled=0
			Leftdlight1.state=2
			I "ZoneLeft1_Hit() vel=" & CStr(Activeball.velY) & "velX=" & CStr(Activeball.velX)
			BouncePassRTimer.interval=500:BouncePassRTimer.Enabled=1
	    End If
	End If
End Sub

Sub BouncePassLTimer_Timer
	D "BouncePassLTimer"
    TriggerLF001.Enabled=1 '(Gets Enabled above in ZoneLeft_Unhit)
    TriggerRF001.Enabled=1
    ZoneRight.Enabled=1
    ZoneLeft.Enabled=1
	Rightdlight1.state=0
    BouncePassLTimer.Enabled=0
End Sub

Sub TriggerLF001_Hit
'	D "TriggerLF001_Hit - left flip"
	Table1_KeyDown(LeftFlipperKey)
	TimerLF.interval=150+int(RND()*70):TimerLF.Enabled=1
End Sub

Sub TimerLF_Timer
'	D "TimerLF"
	Table1_KeyUp(LeftFlipperKey)
    TimerLF.enabled=0
    ZoneLeft.Enabled=1
End Sub

Sub LeftPostPass()
'	D "LeftPost "
	TimerLShot2.interval=152:TimerLShot2.Enabled=1  '139, 141, 143, 147 149 153
	TriggerRF001.enabled=0
	TriggerLF001.enabled=0
End Sub

Sub PostPassLDelayTimer_Timer
'	D "PostPassLDelayTimer_"
	PostPassLDelayTimer.Enabled=0
	Table1_KeyDown(RightFlipperKey)
    TriggerLF001.enabled=1
End Sub

Sub CradleLogicLResetTimer_Timer
'	D "CradleLogicLRestTimer_"
	ZoneLeft.Enabled=1:ZoneRight.Enabled=1
	Table1_KeyUp(LeftFlipperKey)
	CradleLogicLResetTimer.Enabled=0
	If AutoShot <> "SHOT099" Then Eval(AutoShot).intensity=0
End Sub

' right
Sub  ZoneRight_Hit
'	D "ZoneRight_Hit() - check"
    If BallsOnPlayfield = 1 Then  
       If Activeball.velX<4 and Activeball.velX>-4 Then
		     If Activeball.velY<15.5 and Activeball.velY>-15.5 Then 
				D "ZoneRight_Hit() - press flipper"
				Table1_KeyDown(RightFlipperKey)
				CradleRight.Enabled=1
				TriggerRF001.enabled=0
		    End If
	     End If
    End if
End Sub

Sub ZoneRight_Unhit
'	D "ZoneRight_unhit - release flipper"
	Table1_KeyUp(RightFlipperKey)
	CradleRight.Enabled=0:CradleRIghtTimer.Enabled=0
	TriggerRF001.enabled=1
End Sub

Sub CradleRight_Hit
D "cradleRight"
	CradleRightTimer.interval=750:CradleRightTimer.Enabled=1
	If BallsOnPlayfield=1 Then bPauseTimer=True
End Sub
Sub CradleRight_UnHit
	CradleRightTimer.Enabled=0
	bPauseTimer=False
End Sub

Sub CradleRightTimer_Timer
'	D "CradleRightTimer_ release flipper"
	CradleRightTimer.Enabled=0
	Table1_KeyUp(RightFlipperKey)
	TriggerRF001.enabled=0
	CheckShotPriority 1
End Sub


Sub ZoneRight1_Hit
	If BouncePassLTimer.Enabled = True Then Exit Sub
    If Activeball.velY>13.7 and Activeball.velY<18 and BallsOnPlayfield = 1 Then
		D2 "ZoneRight1_Hit() vel=" & CStr(Activeball.velY) & "velX=" & CStr(Activeball.velX)
		If Activeball.velX<-2 Then
			D2 "Deadflip"
			TriggerLF001.Enabled=0
			TriggerRF001.Enabled=0
			ZoneLeft.Enabled=0
			ZoneRight.Enabled=0
			Rightdlight1.state=2
			I "ZoneRight1_Hit() vel=" & CStr(Activeball.velY) & "velX=" & CStr(Activeball.velX)
			BouncePassLTimer.interval=500:BouncePassLTimer.Enabled=1
	    End If
	End If
End Sub

Sub BouncePassRTimer_Timer
	D "BouncePassRTimer"
    TriggerLF001.enabled=1 '(Gets Enabled above in ZoneLeft_Unhit)
    TriggerRF001.Enabled=1
    ZoneRight.Enabled=1
    ZoneLeft.Enabled=1
	Leftdlight1.state=0
    BouncePassRTimer.Enabled=0
End Sub

Sub TriggerRF001_Hit
	D "TriggerRF001_Hit - left flip"   'aa
	Table1_KeyDown(RightFlipperKey)
	TimerRF.interval=150+int(RND()*70):TimerRF.Enabled=1
End Sub

Sub TimerRF_Timer
	D "TimerRF - release RightFlipper"
	Table1_KeyUp(RightFlipperKey)
    TimerRF.enabled=0
    ZoneRight.Enabled=1
End Sub

Sub RightPostPass()
	D "RightPost "
	TimerRShot2.interval=135:TimerRShot2.Enabled=1   'was 140, 130
	TriggerRF001.enabled=0
	TriggerLF001.enabled=0
End Sub

Sub PostPassRDelayTimer_Timer
	D "PostPassRDelayTimer_"
	PostPassRDelayTimer.Enabled=0
	Table1_KeyDown(LeftFlipperKey)
    TriggerRF001.enabled=1
End Sub

Sub CradleLogicRResetTimer_Timer
	D "CradleLogicRRestTimer_"
	ZoneLeft.Enabled=1:ZoneRight.Enabled=1
	Table1_KeyUp(RightFlipperKey)
	CradleLogicRResetTimer.Enabled=0
	If AutoShot <> "SHOT119" Then Eval(AutoShot).intensity=0
End Sub

Sub TimerLShot2_Timer '(Post Pass)
	D "TimerLShot2_"
	TimerLShot2.Enabled=0
	TriggerRF001.enabled=0
	Table1_KeyDown(LeftFlipperKey)
	CradleLogicLResetTimer.interval=450:CradleLogicLResetTimer.Enabled=1
	CradleRight.Enabled=1
	PostPassLDelayTimer.interval=450:PostPassLDelayTimer.Enabled=1   ' 1500  was 700
End Sub

Sub TimerRShot2_Timer '(Post Pass)
	D "TimerRShot2_"
	TimerRShot2.Enabled=0
	TriggerLF001.enabled=0
	Table1_KeyDown(RightFlipperKey)
	CradleLogicRResetTimer.interval=450:CradleLogicRResetTimer.Enabled=1
	CradleLeft.Enabled=1
	PostPassRDelayTimer.interval=450:PostPassRDelayTimer.Enabled=1   ' 1500
End Sub

'********************************** AI Shot Priority ****************************************

Dim NextShotR:NextShotR=1
Dim NextShotL:NextShotL=1
Dim AutoShot
Dim fudgefactor:fudgefactor=1

Sub CheckShotPriority(flipper) 'flipper: 0 = left flipper; 1 = right flipper  
	If flipper = 0 Then
		ZoneLeft.Enabled=0
	Else
		ZoneRight.Enabled=0
	End If

	If flipper = 0 Then
		NextShotL=4
		If i107.state=2 then 
			NextShotL=4 ' right orbit
		Else
			If i103.state<>0 Then
				NextShotL=5
			ElseIf i86.state<>0 or F149.state<>0 or F143.state<>0  Then
				NextShotL=8
			ElseIf i34.state<> 0 Then ' next Song
				NextShotL=99
			ElseIf i94.state<>0 Then  ' lock elevator
				NextShotL=7
			ElseIf i100.state<>0 Then  ' lock toybox
				NextShotL=6
			ElseIf i107.state<>0 Then  ' right orbit
				NextShotL=4
			ElseIf i97.state<>0 Then 
				NextShotL=6
			ElseIf i91.state<>0 Then 
				NextShotL=7
			ElseIf i45.state=0 Then  'aerosmith
				NextShotL=3
			ElseIf i81.state<>0 Then
				NextShotL=9
			ElseIf I80.state=0 Then ' go for ToyBox
				NextShotL=9
			ElseIf i44.state=0 Then
				NextShotL=2
			ElseIf i43.state=0 Then
				NextShotL=1
			Else 
				NextShotL=99
			End If
	'NextShotL=99 '' debug
		End If
		If BallsOnPlayfield > 1 then NextShotL=8
'1ssssss
		select Case NextShotL
			Case 1:shotval=560		' t8
			Case 2:shotval=555		' t7
			Case 3:shotval=553		' t6
			Case 4:shotval=547		' r orbit
			Case 5:shotval=526		' r ramp    '499 497 508 512
			Case 6:shotval=496		' switch   ' 440 500 490 489
			Case 7:shotval=485		' elevator   ' 413
			Case 8:shotval=447		' c ramp   '379  384
			Case 9:shotval=250		' toy box
			Case 10:shotval=240		' t5   220
			Case 11:shotval=208		' t4
			Case 99:shotval=0 		' leftpostpass
		End Select
		AutoShot="SHOT" & LPAD(CSTR(NextShotL),3,"0")
		If NextShotL = 99 Then
			LeftPostPass
		Else
			D AutoShot & " ShotVal=" & CSTR(shotval)
			SoundStrobe AutoShot
			D "****SHOTTIMERLEFT=" & shotval
			TimerLshot1.interval=shotval*fudgefactor
			TimerLShot1.Enabled=1
		End If
	End If

	If flipper = 1 Then
		NextShotR=9
		If i91.state<>0 Then
			NextShotR=10
		ElseIf i97.state=2 Then
			NextShotR=11 ' switch
		ElseIf i81.state<>0 Then
			NextShotR=8
		ElseIf i62.state<>0 Then
			NextShotR=5
		ElseIf i34.state<> 0 Then ' next Song
			NextShotR=4
		ElseIf i35.state<>0 Then
			NextShotR=4
		ElseIf i69.state=0 Then
			NextShotR=6
		ElseIf i70.state=0 Then
			NextShotR=7
		ElseIf i31.state=0 Then
			NextShotR=3
		ElseIf i86.state<>0 or F149.state <> 0 Then
			NextShotR=9    ' 
		ElseIf i30.state=0 Then
			NextShotR=2
		ElseIf (i72.state=0 or i73.state=0) and I75.state=0 Then
			NextShotR=8
		ElseIf i29.state=0 Then
			NextShotR=1
		Else	
			NextShotR=99
		End If
		If BallsOnPlayfield > 1 then NextShotR=9
		select Case NextShotR
			Case 1:shotvalRight=541		' t1
			Case 2:shotvalRight=535		' t2   533
			Case 3:shotvalRight=529 	' t3
			Case 4:shotvalRight=510		' CIU   510
			Case 5:shotvalRight=483   	' left orbit  489
			Case 6:shotvalRight=469		' t
			Case 7:shotvalRight=459		' t   445
			Case 8:shotvalRight=425		' toybox   407
			Case 9:shotvalRight=380		' c ramp   ' 376
			Case 10:shotvalRight=343	' elevator  285 291 294
			Case 11:shotvalRight=284	' switch    ' 279
			Case 99:shotval=0			' rightpostpass
		End Select
		AutoShot="SHOT" & LPAD(CSTR(20+NextShotR),3,"0")
		If NextShotR = 99 Then
			RightPostPass
		Else
			D AutoShot & " val=" + CStr(shotvalRight)
			SoundStrobe AutoShot
			NextShotR=NextShotR+1
			D "****SHOTTIMERRIGHT=" & shotvalRight		
			TimerRshot1.interval=shotvalRight*fudgefactor
			TimerRShot1.Enabled=1
		End If
	End If
End Sub

Sub TimerLShot1_Timer 
	Table1_KeyDown(LeftFlipperKey)
	If AutoShot <> "SHOT099" Then Eval(AutoShot).intensity=2000
	TimerLShot1.Enabled=0:CradleLogicLResetTimer.Enabled=1
End Sub

Sub TimerRShot1_Timer 
	Table1_KeyDown(RightFlipperKey)
	If AutoShot <> "SHOT099" Then Eval(AutoShot).intensity=2000
	TimerRShot1.Enabled=0:CradleLogicRResetTimer.Enabled=1
End Sub


Sub SoundStrobe(Shot)   
		Eval(Shot).intensity=1501
End Sub

'*****************************************************************
'   **********  PUPDMD  MODIFY THIS SECTION!!!  ***************
'PUPDMD Layout for each Table1
'Setup Pages.  Note If you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS  "Case sensitive exact naming fonts!"
'*****************************************************************

Sub pSetPageLayouts

	DIM dmddef
	DIM dmdalt
	DIM dmdscr
	DIM dmdfixed
	DIM digitLCD
	DIM dmdFont


'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
'***********************************************************************'
'<screen#>, in standard wed set this to pDMD ( or 1)
'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
'<fontName> Windows font name, this must be exact match of OS front name. If you are using custom TTF fonts then double check the name of font names.
'<size%>, Height as a percent of display height. 20=20% of screen height.
'<colour>, integer value of windows color.
'<rotation>, degrees in tenths   (900=90 degrees)
'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but If you want to force a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'<PageNum> IMPORTANT this will assign this label to this page or group.
'<visible> initial state of label. visible=1 show, 0 = off.

' Overlay
	'puPlayer.LabelInit pOverlayFrame

' Backglass - this is basically the FullLCD-DMD
'		------------------------------------------------------------------------
' Prgrs |           		  										 Ball
' 		|       						MessageT			    	MTimer
'		|  Time
'		| 		    
'		| 								 Message					Aerosmith
'		|							     							Timer
'		|	                            Play1Score
'		|	      Play1Score                            Play2Score
'		|	Play1Score                  Play2Score			       Play3Score
'		|	Play1Score       Play2Score			Play3Score	          Play4Score
'		-------------------------------------------------------------------------
'
	digitLCD=DigitFont
    dmdFont=DMDMainFont
  

'					   Scrn LblName    Fnt    			Size	Color	 						R,AxAy,X,Y,pagenum,Visible 
	PuPlayer.LabelInit pBackglass
	PuPlayer.LabelNew pBackglass,"ModeProgress", DMDScrFont,10*FontScale,RGB(255, 255, 255)  	,0,0,0 ,0,0    ,1,0					' Mode Progress Image (NOT VISIBLE)
	PuPlayer.LabelSet pBackglass,"ModeProgress", "",0,"{'mt':2,'color':111111,'width':2, 'height':2, 'yalign':0,'ypos':1.0,'xpos':0,'pagenum':1}"
	puPlayer.LabelNew pBackglass,"Aerosmith",DMDScrFont,	7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1					


	puPlayer.LabelNew pBackglass,"MessageT",DMDScrFont,		8*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,16    ,1,1						' Top middle message
	puPlayer.LabelNew pBackglass,"Ball"    ,DMDScrFont,		8*FontScale,RGB(255, 255, 255) 	,0,1,0 ,88,1    ,1,1
	puPlayer.LabelNew pBackglass,"MTimer"  ,DMDScrFont,		10*FontScale,RGB(255, 255, 255)	,0,2,0 ,85,18   ,1,1						' PF Multiplier


	PuPlayer.LabelNew pBackglass,"JesterImage",DMDScrFont,		7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1	
	PuPlayer.LabelSet pBackglass,"JesterImage","PupOverlays\\clear.png",1,"{'mt':2,'width':100,'height':100,'xpos':0,'ypos':0}"

	PuPlayer.LabelNew pBackglass,"Jester"	,DMDScrFont,	 8*FontScale,RGB(255,255,80)	,0,2,0,7.2,45.2	,1,1

	PuPlayer.LabelNew pBackglass,"DiceImage",DMDScrFont,	 7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1
	PuPlayer.LabelSet pBackglass,"DiceImage","PuPOverlays\\clear.png",1,"{'mt':2,'width':100,'height':100,'xpos':0,'ypos':0}"
	
	PuPlayer.LabelNew pBackglass,"SpinHits"	,DMDScrFont,	 8*FontScale,RGB(255,255,255)	,0,2,0,7.2,57.8 ,1,1

	PuPlayer.LabelNew pBackglass,"CoinImage",DMDScrFont,	7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1
	PuPlayer.LabelSet pBackglass,"CoinImage","PupOverlays\\clear.png",1,"{'mt':2,'width':100,'height':100,'xpos':0,'ypos':0}" 

	PuPlayer.LabelNew pBackglass,"Coins"	,DMDScrFont,	 8*FontScale,RGB(255,255,255)	,0,2,0,98.4,32.2,1,1

	PuPlayer.LabelNew pBackglass,"SpinScore",DMDScrFont,	 5*FontScale,RGB(255,255,255)	,0,2,0,5.7,70	,1,1
	PuPlayer.LabelNew pBackglass,"PopHits"	,DMDScrFont,	 8*FontScale,RGB(255,255,80)	,0,2,0,99.5,47	,1,1
	PuPlayer.LabelNew pBackglass,"PopScore"	,DMDScrFont,	 5*FontScale,RGB(255,255,255)	,0,2,0,99.5,66	,1,1

	puPlayer.LabelNew pBackglass,"Timer"   ,DMDScrFont,		10*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,75    ,1,1						' Used for the counddown on the hurry up
	puPlayer.LabelNew pBackglass,"Message" ,DMDScrFont,		18*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,65    ,1,1						' Shows how many points we got on the hurry up timer
	puPlayer.LabelNew pBackglass,"Time"	   ,DMDScrFont,		10*FontScale,RGB(255, 255, 255) ,0,1,0,11.2,11.3,1,1						' Shows remaining time 

	puPlayer.LabelNew pBackglass,"Message1",DMDScrFont,		15*FontScale,RGB(181, 152, 206)	,0,1,1 ,0,25   ,1,1
	puPlayer.LabelNew pBackglass,"Message2",DMDScrFont,		18*FontScale,RGB(20, 20, 255)	,0,1,1 ,0,50   ,1,1
	puPlayer.LabelNew pBackglass,"Message3",DMDScrFont,		10*FontScale,RGB(255, 25, 0)	,0,1,1 ,0,70   ,1,1

	puPlayer.LabelNew pBackglass,"BigScore",DMDScrFont,		18*FontScale,RGB(255, 255, 255)	,0,1,1 ,50,50  ,1,1
	puPlayer.LabelNew pBackglass,"Play1Score",DMDScrFont,	 8*FontScale,RGB(255, 255, 255)	,0,1,2 ,0 ,98  ,1,1
	puPlayer.LabelNew pBackglass,"Play2Score",DMDScrFont,	 8*FontScale,RGB(255, 255, 255) ,0,1,2 ,30,98  ,1,1 
	puPlayer.LabelNew pBackglass,"Play3Score",DMDScrFont,	 8*FontScale,RGB(255, 255, 255) ,0,1,2 ,60,98  ,1,1
	puPlayer.LabelNew pBackglass,"Play4Score",DMDScrFont,	 8*FontScale,RGB(255, 255, 255) ,0,1,2 ,90,98  ,1,1

	PuPlayer.LabelNew pBackglass,"HighScore1",dmdFont,  	12*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,25   ,2,1	
	PuPlayer.LabelNew pBackglass,"HighScore2",dmdFont,  	12*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,45   ,2,1	
	PuPlayer.LabelNew pBackglass,"HighScore3",dmdFont,  	12*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,75   ,2,1			
	PuPlayer.LabelShowPage pBackglass, 2,0,""

	PuPlayer.LabelNew pBackglass,"EnterHS1",dmdFont,   		14*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,25   ,3,1				' HS Entry
	PuPlayer.LabelNew pBackglass,"EnterHS2",dmdFont,  		14*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,45   ,3,1				' HS Entry
	PuPlayer.LabelNew pBackglass,"EnterHS3",dmdFont,  		14*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,75   ,3,1				' HS Entry

	' Page 1 (OverVideo)
	PuPlayer.LabelInit pOverVid
	PuPlayer.LabelNew pOverVid,"OverMessage1",dmdFont,  	8*FontScale,RGB(250,250,250)  	,0,1,1 ,0,25   ,1,1			' Attract mode and other text over a video
	PuPlayer.LabelNew pOverVid,"OverMessage2",dmdFont,  	10*FontScale,RGB(250,250,250)  	,0,1,1 ,0,50   ,1,1			' 
	PuPlayer.LabelNew pOverVid,"OverMessage3",dmdFont,  	 8*FontScale,RGB(250,250,250)  	,0,1,1 ,0,70   ,1,1			' 
	PuPlayer.LabelShowPage pOverVid, 1,0,""

	' Page 1 (OverVideo)
	PuPlayer.LabelInit pPopUP5
	PuPlayer.LabelNew pPopUP5,"TOverMessage1",dmdFont,  	12*FontScale,RGB(255, 255, 255) ,0,1,1 ,0,25  ,1,1			' Transparent Over Video
	PuPlayer.LabelNew pPopUP5,"TOverMessage2",dmdFont,  	15*FontScale,RGB(255, 255, 255) ,0,1,1 ,0,50  ,1,1			' 
	PuPlayer.LabelNew pPopUP5,"TOverMessage3",dmdFont,  	 8*FontScale,RGB(255, 255, 255) ,0,1,1 ,0,70  ,1,1			' 
	PuPlayer.LabelShowPage pOverVid, 1,0,""


' Page 3 (OverVideo - Finish Mode)
'		-------------------------------------------------------------------------
'		| 							 
'		|	Scattered 
'		|
'		-------------------------------------------------------------------------
	puPlayer.LabelNew pOverVid,"F0",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F1",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F2",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F3",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F4",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F5",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F6",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F7",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F8",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 


' Page 1 (Bonus)
'		-------------------------------------------------------------------------
'		| 							    MessageT							 
'		|  							    
'		|   
'		|
'		|								 Message1
'		|							     Message2
'		|
'		-------------------------------------------------------------------------
'
	puPlayer.LabelInit pBonusScreen

	puPlayer.LabelNew pBonusScreen,"Message1" ,DMDScrFont,		15*FontScale,RGB(255, 255, 0)  	,0,1,0 ,0,25   ,1,1						' Shows the Bonus
	puPlayer.LabelNew pBonusScreen,"Message2" ,DMDScrFont,		15*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,70   ,1,1						' Shows the bonus 2nd line
	PuPlayer.LabelShowPage pBonusScreen,1,0,""

	If PuPDMDDriverType=pDMDTypeLCD THEN  'Using 4:1 Standard ratio LCD PuPDMD  ************ lcd **************
		D "pDMDTypeLCD"
		dmdalt="PKMN Pinball"    
		dmdfixed="Instruction"
		dmdscr="Impact"  'main score font
		dmddef="Impact"

	'Page 1 (default score display)
	'					   Scrn LblName    Fnt    Size	Color	 R AxAy X Y pagenum Visible 
		 PuPlayer.LabelNew pDMD,"Player"  ,dmddef,21*FontScaleDmd	,RGB(3, 57, 252)	,1,0,0, 0,0,	1,	0
		 PuPlayer.LabelNew pDMD,"Ball"    ,dmddef,21*FontScaleDmd	,RGB(3, 57, 252)	,1,2,0, 0,0,	1,	0
		 PuPlayer.LabelNew pDMD,"CurScore",dmddef,50*FontScaleDmd	,RGB(255, 255, 255),0,1,2, 0,60,	1,	0
		 PuPlayer.LabelNew pDMD,"Status"  ,dmddef,30*FontScaleDmd	,RGB(255, 255, 255)	,0,0,2, 0,0,	1,	0
 		 PuPlayer.LabelNew pDMD,"Credits" ,dmddef,21*FontScaleDmd	,RGB(3, 57, 252)	,0,2,2, 0,0,	1,	0


	'Page 2 (default Text Splash 1 Big Line)
		PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,40*FontScaleDmd,33023,0,1,1,0,0,2,0

	'Page 3 (default Text 3 Lines)
		PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30*FontScaleDmd,8454143,0,1,0,0,2,3,0
		PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30*FontScaleDmd,33023,0,1,0,0,30,3,0
		PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25*FontScaleDmd,33023,0,1,0,0,57,3,0


	'Page 4 (default Text 2 Line)
		PuPlayer.LabelNew pDMD,"Splash4a",dmddef,40*FontScaleDmd,8454143,0,1,0,0,0,4,0
		PuPlayer.LabelNew pDMD,"Splash4b",dmddef,30*FontScaleDmd,33023,0,1,2,0,75,4,0

	'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
		PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80*FontScaleDmd,8421504,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80*FontScaleDmd,65535  ,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80*FontScaleDmd,65535  ,0,1,1,0,0,5,0

	'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
		PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90*FontScaleDmd,65280,0,0,0,15,1,6,0
		PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50*FontScaleDmd,33023,0,1,0,60,0,6,0
		PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40*FontScaleDmd,33023,0,1,0,60,50,6,0

	'Page 7 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20*FontScaleDmd,8454143,0,1,0,0,2,7,0
		PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40*FontScaleDmd,33023,0,1,0,0,20,7,0
		PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40*FontScaleDmd,33023,0,1,0,0,50,7,0


	END IF  ' use PuPDMDDriver
End Sub


Sub pDMDStartUP
	D "pDMDStartUp"
	pupDMDDisplay "attract","Welcome","@vidIntro.mp4",9,0,10
	pInAttract=true
End Sub

' Background base
const kDMD_Attract			=454  ' blank.png  -   gotg=default.mp4
const kDMD_PlayerMode		=456  ' PlayerOverlay

const kDMD_Mystery	=599


const kDMD_BonusBG			=479   ' blank.png
const kDMD_BonusClear		=480   ' blank.png

const kDMD_last		=521
const kDMD_walk		=522
const kDMD_same		=523
const kDMD_sweet	=524
const kDMD_dude		=525
const kDMD_back		=526
const kDMD_rats		=527
const kDMD_toys		=528
const kDMD_love		=529

'DWE
'************************ called during gameplay to update Scores ***************************
Sub pUpdateScores  'call this ONLY on timer 300ms is good enough
Dim StatusStr
	'D "pUpdateScores PlayerMode:" & PlayerMode &  " 2:" & PlayerMode2 & "BonusMode:" & BonusMode

	StatusStr=""

	if tmrMedleyTour.Enabled  then
		if WizardHits > 0 Then
		Select case PlayerMode3:
			case 0:	StatusStr = "LOOPS LEFT:" & WizardHits
			case 1:	StatusStr = "SHOTS LEFT:" & WizardHits
			case 2: StatusStr = "TARGETS LEFT:" & WizardHits
			case 3: StatusStr = "RAMPS LEFT:" & WizardHits
			case 4: StatusStr = "LANES LEFT:" & WizardHits
			case 5: StatusStr = "POPS LEFT:" & WizardHits
			case 6: StatusStr = "SPINS LEFT:" & WizardHits
			case 7: StatusStr = "TOYS LEFT:" & WizardHits
			case 8: StatusStr = "ELEVATOR LEFT:" & WizardHits
		End Select
		End If
	elseif tmrFinalTour.Enabled  then 
		StatusStr = "Shots Remaining:" & WizardHits

	elseif PlayerMode = -1 Then
		StatusStr="Select Song"
		if bMedleyTourReady Then
			StatusStr = ""
		elseIf bSecondMode then
			StatusStr = ""
		elseIf  bBonusMode Then
			StatusStr = ""
		elseif bToyBoxMultiball Then
			if ToyBoxMBJackpotHits < 18 then
				StatusStr = "TOYS LEFT:" & 18-ToyBoxMBJackpotHits
			Else	
				StatusStr = ""
			End If
		elseIf bMultiBallMode Then 
			StatusStr = ""
		else
			StatusStr = "Select Song"
		End If 
	else
		StatusStr = "Time:" & ModeCountdownTimer.UserValue & "  " & ModePercent(PlayerMode) & "%"
	end If

	PuPlayer.LabelSet pBackglass,"Ball",	"BALL " & CStr(Balls),1,""

	If bWizardMode Then
		PuPlayer.LabelSet pBackglass,"Coins"    ,"",0,""
		PuPlayer.LabelSet pBackglass,"Jester"   ,"",0,""
		PuPlayer.LabelSet pBackglass,"SpinHits" ,"",0,""
		PuPlayer.LabelSet pBackglass,"SpinScore","",0,""
		PuPlayer.LabelSet pBackglass,"PopHits"  ,"",0,""
		PuPlayer.LabelSet pBackglass,"PopScore" ,"",0,""
		PuPlayer.LabelSet pBackglass,"Coins"    ,"",0,""
	Else
		PuPlayer.LabelSet pBackglass,"Jester"   ,CStr(HiddenJesters),1,""
		PuPlayer.LabelSet pBackglass,"SpinHits" ,CStr(spinHits),1,""
		If SpinScore > 10000 Then
			PuPlayer.LabelSet pBackglass,"SpinScore" ,CSTR(int(SpinScore/1000))+"K",1,""
		Else
			PuPlayer.LabelSet pBackglass,"SpinScore" ,CStr(SpinScore),1,""
		End IF
		PuPlayer.LabelSet pBackglass,"PopHits"  ,CStr(PopHits),1,""
		PuPlayer.LabelSet pBackglass,"PopScore" ,CStr(PopScore),1,""
		PuPlayer.LabelSet pBackglass,"Coins" ,CStr(coins),1,""
	End If

	If playermode = -1 and not bToyBoxMultiBall and not bElevMultiBall and not bBallInPlungerLane and score(currentplayer) <> 0 then
		' todo need to be sure we are not showing a video
		If not PauseBigScore  and not bsecondMode  and not bWizardMode Then
			PuPlayer.LabelSet pBackglass,"BigScore",FormatScore(Score(CurrentPlayer)),1,""
		End If
	End If

	Select Case PlayersPlayingGame
		Case 1:
			puPlayer.LabelSet pBackglass,"Play1Score",FormatScore(Score(0)),1,""
		Case 2:
			PuPlayer.LabelSet pBackglass,"Play1Score",FormatScore(Score(0)),1,""
			PuPlayer.LabelSet pBackglass,"Play2Score",FormatScore(9999999),1,""
		Case 3:
			PuPlayer.LabelSet pBackglass,"Play1score",FormatScore(Score(0)),1,""
			PuPlayer.LabelSet pBackglass,"Play2score",FormatScore(Score(1)),1,""
			PuPlayer.LabelSet pBackglass,"Play3score",FormatScore(99999999),1,""
		Case 4:
			PuPlayer.LabelSet pBackglass,"Play1score",FormatScore(Score(0)),1,""
			PuPlayer.LabelSet pBackglass,"Play2score",FormatScore(Score(1)),1,""
			PuPlayer.LabelSet pBackglass,"Play3score",FormatScore(Score(2)),1,""
			PuPlayer.LabelSet pBackglass,"Play4score",FormatScore(99999999),1,""
	End Select 

	PuPlayer.LabelSet pDMD,"Player",	"Player " & CurrentPlayer+1				,1,""
	PuPlayer.LabelSet pDMD,"Ball",		"BALL " & CStr(Balls)					,1,""
	PuPlayer.LabelSet pDMD,"CurScore",	FormatScore(Score(CurrentPlayer))		,1,""
	puPlayer.LabelSet pDMD,"Status",	StatusStr								,1,"1"
	PuPlayer.LabelSet pDMD,"Credits", 	"C  " & Credits							,1,""


End Sub

' This is called when they hit the plunger and when they press start after ball is in the scoop
Sub StartPlayerMode()
	dim time
	D "StartPlayerMode " & PlayerMode
	pDMDSetPage(pScores)
	setCIULight(False)    ' Gets set after the first shot
	setModeSelectLight(False)

	bSecondMode = False
	bBonusMode = False

	time = 1000
'	sndGeneralHit = ""
	playclear pAudio
	ModePoints = 0
	bModeProgressUpgraded = False

	bPlayerModeSelect = False
	pBGGamePlay

	If bUsePUPDMD then 
		D ">>>>> Clear the MESSAGE <<<<<"
		PuPlayer.LabelSet pBackglass,"MessageT"," "	,0,"{'mt':1,'at':1,'fq':250 }"
	end if

	If (PlayerMode <> -1) then 					' Add this to the stack
		StackState(kStack_Pri0).Enable(PlayerMode)
	End If

	Select Case PlayerMode
		Case -1:' No Mode Selected
		Case 0: 
			DMD "Start Mode", CL(0, "LAST CHILD"), "", eNone, eNone, eNone, time, False, ""
			SSetLightColor kStack_Pri0, I62,  modeRampColor, 2
			SSetLightColor kStack_Pri0, I107, modeRampColor, 2
			SSetLightColor kStack_Pri0, I86,  modeRampColor, 1
			SSetLightColor kStack_Pri0, I103, modeRampColor, 1
		Case 1: 
			DMD "Start Mode", CL(0, "WALK THIS WAY"), "", eNone, eNone, eNone, time, False, ""
			SSetLightColor kStack_Pri0, I86, modeRampColor, 2
			SSetLightColor kStack_Pri0, I107,modeRampColor, 1
			SSetLightColor kStack_Pri0, I62, modeRampColor, 1
		Case 2: 
			DMD "Start Mode", CL(0, "SAME OLD"), "", eNone, eNone, eNone, time, False, ""
			SSetLightColor kStack_Pri0, aRampLights(bRndModeShot),  modeRampColor, 2
		Case 3: ' sweet
			DMD "Start Mode", CL(0, "SWEET EMOTION"), "", eNone, eNone, eNone, time, False, ""
			SSetLightColor kStack_Pri0, I35, modeRampColor, 2
			SSetLightColor kStack_Pri0, I81, modeRampColor, 2
			SSetLightColor kStack_Pri0, I86, modeRampColor, 2
			SSetLightColor kStack_Pri0, I91, modeRampColor, 2
			SSetLightColor kStack_Pri0, I97, modeRampColor, 2
			SSetLightColor kStack_Pri0, I103,modeRampColor,2
		Case 4: 
			DMD "Start Mode", CL(0, "DUDE"), "", eNone, eNone, eNone, time, False, ""
			SSetLightColor kStack_Pri0, I86, modeRampColor, 1
			SSetLightColor kStack_Pri0, I91, modeRampColor, 1
		Case 5: 
			DMD "Start Mode", CL(0, "BACK IN THE SADDLE"), "", eNone, eNone, eNone, time, False, ""
			SSetLightColor kStack_Pri0, I107, modeRampColor, 1
		Case 6: 
			DMD "Start Mode", CL(0, "RATS IN THE CELLAR"), "", eNone, eNone, eNone, time, False, ""
			SSetLightColor kStack_Pri0, I86,  modeRampColor, 1
	End Select

	StartPlayerModeVideo False

	If PlayerMode <> -1 then 
		ModeCountdownTimer.UserValue = ModeCountdown
D "kDMD_PlayerMode:4"
		pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1)))
		vpmtimer.addtimer cInt(defBallSaverTime/4), "ModeCountdownTimer.Enabled = True '" ' delay the countdown
	End If 
	D "StartPlayerMode calling SetModeLights"
	SetModeLights()
End Sub


Sub StopPlayerMode()
	D "StopPlayerMode " & PlayerMode
	Dim i
	ModeCountdownTimer.Enabled = False

	setCIULight(False)
	For i = 0 to 6	' check if one of the modes is not completed then allow them to select
		if ModePercent(i) < 100 then
			D "Still another song available: ModePercent A i=" & i & " %" & ModePercent(i)
			setModeSelectLight(True)
			exit For
		end If
	Next
	sndGeneralHit = ""

	If bUsePUPDMD then 
		' Turn off mode progress 
'		puPlayer.LabelSet pBackglass, "ModeProgress", "",0,""
		' Clear this in Case they finished the mode
		puPlayer.LabelSet pBackglass,"HighScore1", "" ,1,""
		puPlayer.LabelSet pBackglass,"HighScore2", "" ,1,""
		puPlayer.LabelSet pBackglass,"HighScore3", "" ,1,""
	End if

	dim a
	For each a in aRampLights				' Turn off all the ramp lights to reset the mode
		SSetLightColor kStack_Pri0, a, "white", 0
	Next

	Select Case PlayerMode
		Case -1:' No Mode Selected
		Case 0: ' last
        Case 1: ' walk
				ModeProgress(PlayerMode) = 10
				ModePercent(PlayerMode) = 100
				bBonusMode=True
        Case 2: ' same
			If bModeProgressUpgraded Then ' 
				for each a in aerosmithLights
					a.state=0
				Next
			End If
		Case 3: ' sweet
			If bHardMode=True Then
				If ModePercent(PlayerMode) < 100 Then ' We didnt complete the mode timer expired, start over 
					ModeProgress(PlayerMode) = 0  
					ModePercent(PlayerMode) = 0
				End If
			End If  
        Case 4: ' dude
        Case 5: ' back
				ModeProgress(PlayerMode) = 8
				ModePercent(PlayerMode) = 100
				bBonusMode=True
        Case 6: ' rats
				ModeProgress(PlayerMode) = 8
				ModePercent(PlayerMode) = 100
				bBonusMode=True
	End Select

	If ModeProgress(PlayerMode) > 0 Then ' save it in the Array
		for a = 0 to 8
			If ModeOrder(a)=-1 or ModeOrder(a)=PlayerMode Then
				ModeOrder(a)=PlayerMode
				D "Saving PlayerMode " & PlayerMode & " as #" & a & " mode played"
				Exit For
			End If
		Next
	End If

	if not bWizardMode Then
		' Turn off all the Mode Lights
		I15.State = 0:I16.State = 0:I17.State = 0:I18.State = 0
		I19.State = 0:I20.state = 0:I21.State = 0
		D "StopPlayerMode calling SetModeLights"
		SetModeLights()			' Set mode lights based on progress
	End If

	If PlayerMode <> -1 then 
		StackState(kStack_Pri0).Disable  ' Disable this stack level
	End If 

	CheckWizardModesReady		' See If we need to enable wizard modes	
								
	If PlayerMode <> -1 Then	' Start 2nd Mode
		D "2nd Mode? " & ModePercent(PlayerMode) & " 2ndMode:" & bSecondMode & "BMode:" & bBonusMode
		If ModePercent(PlayerMode) >= 100 and bBonusMode then  
			PlayerMode2 = PlayerMode   ' +100 to show we are in ShotMode
			StartShotMode()
		Else 
			If not bElevMultiBall Then StopPlayerModeVideo   ' todo is this condition needed?
			For i = 0 to 6	' check if one of the modes is not completed then allow them to select
				If ModePercent(i) < 100 then
					D "Still another song available: ModePercent B i=" & i & " %" & ModePercent(i)
					setModeSelectLight(True)
					exit For
				End If
			Next
			PuPlayer.LabelSet pBackglass, "ModeProgress", "",0,""  
		End if
	End If 
	PlayerMode = -1
	D "StopPlayerMode calling SetModeLights"
	SetModeLights
End Sub


sub CheckWizardModesReady()
	If bWizardMode then exit sub  ' We are in a wizard mode no need to check to enable one

	D "CheckWizardModesReady.." & modesStarted
	If modesStarted=9 and bMedleyTourDone=False Then	' Medley (all 7 > 0, toybox and elev multi ball atleast had 1 hit/started)
		D "Setup Medley MB"
		If bMedleyTourReady=False then
			bMedleyTourReady = True
			SceneClearMessage()
			SetLightColor I34, "white", 2
			SetLightColor I35, "white", 2
		End If 
		PlayScoopLightSeq
	elseIf modesCompleted=9 and bMedleyTourDone Then	' Complete all modes 
		D "Setup Final Tour"
		bFinalTourReady=True 'bXandarReady = True
		SceneClearMessage()
		SetLightColor I34, "white", 2
		SetLightColor I35, "white", 2
		PlayScoopLightSeq
	End If 
End Sub 


Sub StartShotMode()
	D "StartShotMode Mode:" & PlayerMode & " 2:" & PlayerMode2
' Turn on All Shots
' Get One to go to Super mode
	Dim a
	StackState(kStack_Pri0).Enable(PlayerMode2)
	For each a in aRampLights
		SSetLightColor kStack_Pri0, a, modeRampColor, 2
	Next
End Sub

Sub StartPlayerMode2()
	D "StartPlayerMode2 : " & PlayerMode2 & " bBonusMode:" & bBonusMode
	Dim a
	If PlayerMode2 <> -1 Then
		'StackState(kStack_Pri0).Enable(PlayerMode2)
		For each a in aRampLights
			SSetLightColor kStack_Pri0, a, modeRampColor, 0
		Next

		Mode2Percent(PlayerMode2) = 0
		Mode2Total(PlayerMode2) = 0
		Mode2Progress(PlayerMode2) = 0
	End If
	
	ModePoints = 0
	StartPlayerModeVideo False

	Select Case PlayerMode2:
		Case -1:
		Case 0: ' last
			Mode2Value(PlayerMode2)=500000+(5000*Coins)
			SetLightColor F151, "cyan", 2
			SetLightColor F150, "cyan", 2
			ShowPlayerMode2(PlayerMode2)
		Case 1: ' walk
			Mode2Value(PlayerMode2)=500000+(5000*Coins)
			SetLightColor F143, "orange", 2
			SetLightColor F149, "orange", 2
			ShowPlayerMode2(PlayerMode2)
		Case 2: ' same
			Mode2Value(PlayerMode2)=50000+(5000*Coins)
			FlashLevel(4) = 1 : Flasherflash4_Timer
			ShowPlayerMode2(PlayerMode2)
		Case 3: ' scoring
			Mode2Value(PlayerMode2)=500000+(5000*Coins)
			FlashLevel(2) = 1 : Flasherflash2_Timer
			FlashLevel(3) = 1 : Flasherflash3_Timer
			ShowPlayerMode2(PlayerMode2)
		Case 4:
			Mode2Value(PlayerMode2)=500000+(5000*Coins)
			setDudeLight(True)
			ShowPlayerMode2(PlayerMode2)
		Case 5: ' saddle
			Mode2Value(PlayerMode2)=250000+(5000*Coins)
			SetLightColor F144, "purple", 2
			ShowPlayerMode2(PlayerMode2)
		Case 6: ' rats 
			Mode2Value(PlayerMode2)=50000+(5000*Coins)
			SetLightColor F141, "red", 2
			ShowPlayerMode2(PlayerMode2)
	End Select
End Sub

Sub ShowPlayerMode2(mode)
D " test"
	Select Case mode 
		Case 0: 
			QueueScene "SceneBMessage ""SUPER LOOPS"",""VALUE:" & Mode2Value(mode) & """, ""HITS LEFT: " & cStr(10-mode2progress(mode)) & """ '", 500,2
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 10 Then
				SetLightColor F151, "cyan", 0
				SetLightColor F150, "cyan", 0
			End If 
		Case 1:
			QueueScene "SceneBMessage ""SUPER RAMPS"",""VALUE:" & Mode2Value(mode) & """, ""HITS LEFT: " & cStr(10-mode2progress(mode)) & """ '", 500,2
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 10 Then
				SetLightColor F143, "orange", 0
				SetLightColor F149, "orange", 0
			End If
		Case 2:
			QueueScene "SceneBMessage ""SUPER TARGETS"",""VALUE:" & Mode2Value(mode) & """, ""HITS LEFT: " & cStr(20-mode2progress(mode)) & """ '", 500,2
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 20 Then
				FlashLevel(4) = 0
			End If
		Case 3:
			QueueScene "SceneBMessage ""SUPER SCORING"","""", ""HITS LEFT: "& cStr(100-mode2progress(mode)) & """ '", 100,2
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 100 Then
				FlashLevel(2) = 0 : Flasherflash2_Timer
				FlashLevel(3) = 0 : Flasherflash3_Timer
			End If
		Case 4:
			QueueScene "SceneBMessage ""SUPER LANES"",""VALUE:" & Mode2Value(mode) & """, ""HITS LEFT: " & cStr(10-mode2progress(mode)) & """ '", 500,2
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 10 Then
				setDudeLight(False)
			End If
		Case 5:
			QueueScene "SceneBMessage ""SUPER POPS"",""VALUE:" & Mode2Value(mode) & """, ""HITS LEFT: " & cStr(50-mode2progress(mode)) & """ '", 500,2
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 50 Then
				SetLightColor F144, "purple", 0
			End If
		Case 6:
			QueueScene "SceneBMessage ""SUPER SPINNERS"",""VALUE:" & Mode2Value(mode) & """, ""HITS LEFT: " & cStr(100-mode2progress(mode)) & """ '", 100,2
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 100 Then
				SetLightColor F141, "red", 0
			End If
	End Select
End Sub

Sub StopPlayerModeVideo()	' chooses the wait video 
	D "StopPlayerModeVideo. (Mix Music/video)"	
	If bGameInPlay = false then exit sub  

	If bToyBoxMultiball or bElevMultiBall Then
		D " Still in MB .. so let the MB video continue"
	else
		playclear pBackglass
		playmedia "Video-0x0000.mp4", "PupVideos", pBackglass, "", -1, "", 1, 1
	end If
End Sub 


'dwe2
Sub SelectPlayerMode(keycode)
	Dim cnt
	D "SelectPlayerMode " & (PlayerMode)
	cnt = 0

    If keycode = LeftFlipperKey Then
        PlaySoundVol "fx_Previous", VolDef
		Do
			cnt=cnt+1
			PlayerMode = (PlayerMode + 1) 
			If (PlayerMode > 6) Then PlayerMode=0
		Loop While ModePercent(PlayerMode) >= 100 and cnt<7  and ModePercent(PlayerMode) = -1  'TODO TESTING HERE ONLY SELECT NON STARTED MODES
        UpdatePlayerMode
    End If
    If keycode = RightFlipperKey Then
        PlaySoundVol "fx_Next", VolDef
		Do 
			cnt=cnt+1
			PlayerMode = (PlayerMode - 1)
			If (PlayerMode < 0) then PlayerMode=6
		Loop While ModePercent(PlayerMode) >= 100 and cnt<8
        UpdatePlayerMode
    End If
	D "SelectPlayerMode Done .." & PlayerMode
End Sub

Function GetModeIndex(lightArrowName)  'todo
	GetModeIndex = -1
	Select Case lightArrowName
		Case "I35": 		 	' CIU 
			GetModeIndex = 0
        Case "I62":  			' left orbit
			GetModeIndex = 1
        Case "I81":  			' toy box
			GetModeIndex = 2	
        Case "I86": 			' center ramp
			GetModeIndex = 3 
        Case "I91": 	 		' Elevator
			GetModeIndex = 4
        Case "I97": 			' toybox kicker
			GetModeIndex = 5
        Case "I103": 			' right ramp
			GetModeIndex = 6 
        Case "I107": 			' right orbit
			GetModeIndex = 7
		Case "I94":
			GetModeIndex = 8 	' elevator lock
	End Select
end Function


Sub EndFinalTour()   ' Xandar
	dim a
	if tmrXandar.Enabled Then
		bWizardMode = False
		StopPlayerModeVideo
		'pBGPlayVideoDone						' Stop the video
		PlaySound "YourTimeIsUp"
		setUpgradeLight(False)
		setModeSelectLight(True)

		OrbTarget1.IsDropped = False
		EnableOrb(True)
		
		For each a in aRampLights				' Turn off all the ramp lights to reset the mode
			SSetLightColor kStack_Pri2, a, "white", 0
		Next
		StackState(kStack_Pri2).Disable

		bXandarDone=True
		tmrXandar.Enabled = False 
	end if
End Sub

'********************
' Music
'********************

Dim Song
Song = ""

Const Llast=1
Const Walk=2
Const Same=3
Const Sweet=4
Const Dude=5
Const Back=6
Const Rats=7
Const Toys=8
Const Love=9

Dim bPlayPaused
bPlayPaused = False

Sub PlaySong(name)
	D "PlaySong Name:" & name & " Prior Song:" & song
	Dim PlayLength

	StopSound Song	' Stop the old song
	If name <> "" then Song = name
	PlayLength = -1
	bPlayPaused=False
	If bUsePUPDMD then 			' Use Pup If we have it so we can pause the music
		playmedia song, MusicDir, pMusic, "", -1, "", 1, 1
		exit sub
	Else
		PlaySound Song, PlayLength, VolBGMusic 'this last number is the volume, from 0 to 1
	End If
End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  UTILITY - Video Manager & skipper
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 
'	Const pTopper=0			' show, 
'	Const pDMD=1			' ForceBack
'	Const pBackglass=2		' ForceBack
'	Const pPlayfield=3		' off
'	Const pMusic=4			' Music Only
'	Const pAudio=5			' Music Only
'	Const pCallouts=8		' Music Only
'	Const pOvervid=14		' ForcePopBack
'	Const pGame=15			' ForcePop - In Game mini game

	dim currentqueue
	dim cineon:cineon = 0
	dim skipped:skipped = 0
	dim bCancelNext:bCancelNext=False
	dim bMediaPaused(19)
	dim bMediaSet(19)
	bMediaPaused(0)=False:bMediaPaused(1)=False:bMediaPaused(2)=False:bMediaPaused(3)=False:bMediaPaused(4)=False:bMediaPaused(5)=False
	bMediaPaused(6)=False:bMediaPaused(7)=False:bMediaPaused(8)=False:bMediaPaused(9)=False:bMediaPaused(10)=False:bMediaPaused(11)=False
	bMediaPaused(12)=False:bMediaPaused(13)=False:bMediaPaused(14)=False:bMediaPaused(15)=False:bMediaPaused(16)=False:bMediaPaused(17)=False
	bMediaPaused(18)=False:bMediaPaused(19)=False
	bMediaSet(0)=False:bMediaSet(1)=False:bMediaSet(2)=False:bMediaSet(3)=False:bMediaSet(4)=False:bMediaSet(5)=False
	bMediaSet(6)=False:bMediaSet(7)=False:bMediaSet(8)=False:bMediaSet(9)=False:bMediaSet(10)=False:bMediaSet(11)=False
	bMediaSet(12)=False:bMediaSet(13)=False:bMediaSet(14)=False:bMediaSet(15)=False:bMediaSet(16)=False:bMediaSet(17)=False
	bMediaSet(18)=False:bMediaSet(19)=False
	sub pausemedia(channel) 
		If bUsePUPDMD=False then exit sub 

		D "pause media ch:" & channel & " current:" & bMediaPaused(channel)
		If bMediaSet(channel) = False then Exit Sub 
		If bMediaPaused(channel) = False then
			D "pause"
			PuPlayer.playpause channel
			bMediaPaused(channel)=True
		End If 
	End Sub

	sub resumemedia(channel) 
		If bUsePUPDMD=False then exit sub 
		D "resume media ch:" & channel & " current:" & bMediaPaused(channel)
		If bMediaSet(channel) = False then Exit Sub 
		If bMediaPaused(channel) then
			D "resume"
			PuPlayer.playresume channel
			bMediaPaused(channel)=False
		End If 
	End Sub
			
	sub playclear(chan)
		If bUsePUPDMD=False then exit sub 
		D2 "play clear'd " & chan
		bMediaSet(chan) = False
		bMediaPaused(chan) = False

		If chan = pOverVid then 
			PuPlayer.PlayStop pOverVid
		End If 

		If chan = pAudio Then
			PuPlayer.SetLoop pAudio, 0
			PuPlayer.playstop pAudio
		End If

		If chan = pBonusScreen then 
			PuPlayer.SetBackGround chan, 0
			PuPlayer.SetLoop chan, 0
			PuPlayer.playstop chan
		End If 

		If chan = pMusic Then
			PuPlayer.playstop pMusic
		End If

		If chan = pBackglass Then
			If currentqueue <> "" then 
				bCancelNext = True
				D "Clear is cancelling " & currentqueue
			End If 
			PuPlayer.SetBackGround pBackglass, 0
			PuPlayer.SetLoop pBackglass, 0
			PuPlayer.playstop pBackglass
		End If 
	end Sub


	dim lastvocall:lastvocall=""
	dim noskipper:noskipper=0

	'example playmedia "hs.mp3","audiomultiballs",pAudio,"cineon",10000,"",1,1  // (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'			 playmedia "player1-Sound-0x03E0.wav", MusicDir, pAudio, "", -1, "", 1, 1
'			playmedia "","audio-modes",pCallouts,"Sound-0x0458Walk.mp3",1500,"",1,1  ' 
'			playmedia "","audio-modes",pCallouts,"",1500,"",1,1  ' 
Sub playmedia(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	D "playmedia " & name
		If bUsePUPDMD=False then exit sub 
		bMediaSet(channel) = True
		If audiolevel = 1 Then
			If channel = pBackglass Then
				audiolevel = VolBGMusic
				currentqueue = ""
			ElseIf channel = pCallouts Then
				audiolevel = VolDef
			ElseIf channel = pMusic Then
				audiolevel = VolMusic
			ElseIf channel = pAudio Then
				audiolevel = VolBGMusic 
			ElseIf channel = pOverVid Then
				audiolevel = VolBGMusic  
			end If
			audiolevel = audiolevel * 100
		end If

D "audiolevel=" & audioLevel & " channel=" & channel
		If channel = pCallouts Then
			If lastvocall <> "" and lastvocall = name  then exit sub
		end if
		
		If nextitem = "" Then
			If cinematic = "cineon" Then
				noskipper=1
				vpmtimer.addtimer length, "nextitems '"
			end If
		Else	
			currentqueue = "playclear " &channel& ":playmedia """ & nextitem &""","""&playlist&""","&channel&","""",-1,"""", "&audiolevel&",1 '"
			vpmtimer.addtimer length, "nextitems '"
			'vpmtimer.AddTimer length, "playclear " &channel& ":playmedia """ & nextitem &""","""&playlist&""","&channel&","""",-1,"""", "&audiolevel&",1 '"
			'vpmtimer.addtimer length, "nextitems '"
			'currentqueue = nextitem
		end If
		If cinematic = "cineon" and length <> -1 Then
			D "ChangeVol pMusic for Cineon"

			skipped=0
			'PuPlayer.playpause 4 ' stop then resume the music
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": " & pMusic &", ""FN"":11, ""VL"":20 }"
			vpmtimer.addtimer length, "turnitbackupcine '"
			GiOff
			cineon = 1
		end If
'   PuPlayer.playlistplayex pCallouts,"audioevents","player1-Sound-0x03E0.wav",100, 1
		D "PlayMedia Chnl:" & channel & " PList:" & playlist & " Name:" & name & " AudioLevel:" & audiolevel & " Pri:" & priority & " Len:" & length
		PuPlayer.playlistplayex channel,playlist,name,audiolevel,priority

		If channel = pBackglass then PuPlayer.SetBackGround channel, 1
		If channel = pAudio then PuPlayer.SetLoop channel, 1
		If channel = pBonusScreen then PuPlayer.SetLoop channel, 1

		If channel = pCallouts and length <> -1 Then
			D "ChangeVol pBackglass Volume for Callouts"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":60 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic&",     ""FN"":11, ""VL"":60 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pAudio&",     ""FN"":11, ""VL"":60 }"
			vpmtimer.addtimer length, "turnitbackup '"
		end If

		If channel = pOverVid and length <> -1 Then
			D "ChangeVol pBackglass for OverVid" 
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":50 }"
			vpmtimer.addtimer length, "turnitbackupvid '"
		end If

		If channel = pCallouts Then
			lastvocall=name
		end if
	end sub

	Sub turnitbackup
		D "Change Volume TURNITBACKUP BGMusic:" & VolBGMusic & " Music:" & VolMusic & " Sfx:" & VolSfx

		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":"&VolBGMusic*100&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic &",    ""FN"":11, ""VL"":"&VolMusic*100&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pAudio&",     ""FN"":11, ""VL"":"&VolSfx*100&" }"
	End Sub

	Sub turnitbackupvid
		D "Change Volume4 BGMusic:" & VolBGMusic
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":"&VolBGMusic*100&" }"
	End Sub

	Sub turnitbackupcine
		D "Change Volume3 Music" & VolMusic
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic&", ""FN"":11, ""VL"":"&VolMusic*100&" }"
		D "turnitbackupcine "
	End Sub

	sub holder
	end sub

	sub nextitems
		If bCancelNext then 	' Cancel the last items that was sent to the queue
			D "Cancel Queue"
			bCancelNext=False
			Exit Sub
		End If 
		D "Executing " & currentqueue
		Execute currentqueue
		currentqueue = ""
		bCancelNext = False 	' Items in queue can trigger a cancel
	end Sub

	sub vidskipper_timer		' TBD: Make this work (flippers skip cinematics)
		If cineon = 1  and noskipper = 0 Then
			If ldown = 1 and rdown = 1 Then
				nextitems
				skipped = 1
			end If
		end If
	end Sub


DIM pCurAttractPos: pCurAttractPos=0


'********************** gets called auto each page next and timed already in DMD_Timer.  make sure you use pupDMDDisplay or it wont advance auto.
Sub pAttractNext
	pCurAttractPos=pCurAttractPos+1
	D "pAttractNext " & pCurAttractPos
	If bGameInPlay Then PriorityReset=2000
	If bGameInPlay and pCurAttractPos=1 then pCurAttractPos=2		' During InstantInfo skip Intro
	If bGameInPlay and pCurAttractPos=7 then pCurAttractPos=9		' During InstantInfo skip credits 

	If bUsePUPDMD then 'wipe out the GAME OVER
		puPlayer.LabelSet pBackglass,"EnterHS1", " ",1,""
		puPlayer.LabelSet pBackglass,"EnterHS2", " ",1,""
		puPlayer.LabelSet pBackglass,"EnterHS3", " ",1,""
	End If 
  Select Case pCurAttractPos
'			      Name       Line1^Line2.. 	Video			 T  Flash	Priority
  Case 1:
	D "Case 1"
	pupDMDDisplay "attract", "Insert Coin",	"@vidIntro.mp4" ,9, 1,		10
	PuPlayer.LabelShowPage pOverVid, 1,0,""
	PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x004A.mp4", 1, 1
	PuPlayer.LabelSet pOverVid,"OverMessage1","LAST SCORE",1,"{'mt':2, 'size': 14 }"
	PuPlayer.LabelSet pOverVid,"OverMessage2","",1,""
	PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(Score(0)),1,"{'mt':2, 'size': 14 }"
  Case 2:
	D "Case 2"
	If bGameInPlay=False Then 
		pupDMDDisplay "attract", "Game Over",	"@vidIntro2.mp4",3, 1,		10
		PuPlayer.LabelSet pOverVid,"OverMessage1","",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2","",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage3","",1,""
	End If 
	'test playclear pOverVid
	PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x0048.mp4", 1, 1

	If osbactive = 1 or osbactive = 2 Then
		PuPlayer.LabelSet pOverVid,"OverMessage1","OSB CHAMPION",1,"{'mt':2, 'size': 15 }"
		PuPlayer.LabelSet pOverVid,"OverMessage2",alltimevar(2),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(alltimevar(3)),1,""
	Else 
		PuPlayer.LabelSet pOverVid,"OverMessage1","GRAND CHAMPION",1,"{'mt':2, 'size': 15 }"
		PuPlayer.LabelSet pOverVid,"OverMessage2",HighScoreName(0),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(HighScore(0)),1,""
	End If
  Case 3:
	If bGameInPlay=False Then pupDMDDisplay "attract", "Insert Coin",	"@vidIntro.mp4" ,9, 1,		10
	PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x004A.mp4", 1, 1
	If osbactive = 1 or osbactive = 2 Then
		PuPlayer.LabelSet pOverVid,"OverMessage1","OSB HIGHSCORE #1",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2",alltimevar(4),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(alltimevar(5)),1,""
	Else 
		PuPlayer.LabelSet pOverVid,"OverMessage1","HIGH SCORE #1",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2",HighScoreName(1),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(HighScore(1)),1,""
	End If 
  Case 4:
	PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x004B.mp4", 1, 1
	If bGameInPlay=False Then pupDMDDisplay "attract", "Game Over",	"@vidIntro2.mp4",3, 1,		10
	If osbactive = 1 or osbactive = 2 Then
		PuPlayer.LabelSet pOverVid,"OverMessage1","OSB HIGHSCORE #2",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2",alltimevar(6),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(alltimevar(7)),1,""
	Else 
		PuPlayer.LabelSet pOverVid,"OverMessage1","HIGH SCORE #2",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2",HighScoreName(2),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(HighScore(2)),1,""
	End If 
  Case 5:
	If bGameInPlay=False Then pupDMDDisplay "attract", "Insert Coin",	"@ARS107-Scene-22.mp4" ,9, 1,		10
	PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x0048.mp4", 1, 1
	If osbactive = 1 or osbactive = 2 Then
		PuPlayer.LabelSet pOverVid,"OverMessage1","OSB HIGHSCORE #3",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2",alltimevar(8),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(alltimevar(9)),1,""
	Else 
		PuPlayer.LabelSet pOverVid,"OverMessage1","HIGH SCORE #3",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2",HighScoreName(3),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(HighScore(3)),1,""
	End If 
  Case 6:
	If bGameInPlay=False Then pupDMDDisplay "attract", "Game Over",	"@vidIntro2.mp4",3, 1,		10
	PuPlayer.playlistplayex pOverVid,"PupVideos","Video-0x0048.mp4", 1, 1
	PuPlayer.LabelSet pOverVid,"OverMessage1","GAMES PLAYED",1,""
	PuPlayer.LabelSet pOverVid,"OverMessage2",FormatScore(TotalGamesPlayed),1,"{'mt':2}"
	PuPlayer.LabelSet pOverVid,"OverMessage3"," ",1,""
  Case 7:
	pupDMDDisplay "attract", "Insert Coin", "@vidIntro.mp4",3, 1,		10
	PuPlayer.playlistplayex pOverVid,"PupVideos","Video-0x0049.mp4", 1, 1
	PuPlayer.LabelSet pOverVid,"OverMessage1","CREDITS",1,""
	PuPlayer.LabelSet pOverVid,"OverMessage2","",1,"{'mt':2}"
	PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(Credits),1,""
  Case 8:
	If bGameInPlay=False Then pupDMDDisplay "attract", "Insert Coin",	"@ARS107-Scene-22.mp4" ,9, 1,		10
	PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x0048.mp4", 1, 1
	PuPlayer.LabelSet pOverVid,"OverMessage1","MEDLEY CHAMPION",1,""
	PuPlayer.LabelSet pOverVid,"OverMessage2",HighScoreName(4),1,"{'mt':2}"
	PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(HighScore(4)),1,""
  Case 9:
	If bGameInPlay then
		PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x0047.mp4", 1, 1
		PuPlayer.LabelSet pOverVid,"OverMessage1","",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2","EEEE",1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3","",1,""
	else 
		pCurAttractPos=0
		pAttractNext 'reset to beginning	
	End If
  Case 10:
	If bGameInPlay then
		PuPlayer.playlistplayex pOverVid,"PupVideos","Video-0x004A.mp4", 1, 1
		PuPlayer.LabelSet pOverVid,"OverMessage1","Missles",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2", SmartButtonCount,1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3","",1,""
	else 
		pCurAttractPos=0
		pAttractNext 'reset to beginning	
	End If
  Case 11:
	If bGameInPlay then
		PuPlayer.playlistplayex pOverVid,"PupVideos","Video-0x004A.mp4", 1, 1
		PuPlayer.LabelSet pOverVid,"OverMessage1", "Multipliers", 1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2", "Play: x" & (Multiplier3x * PlayMultiplier),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3", "Bonus: x" & BonusMultiplier,1,""
	else 
		pCurAttractPos=0
		pAttractNext 'reset to beginning	
	End If
  Case Else
    pCurAttractPos=0
    pAttractNext 'reset to beginning
  end Select
End Sub

Sub I(aStr)
	debug.print aStr
	TextBox.Text=aStr
	'objFile.Write aStr & vbCrLf
End Sub

Sub D(aStr)
	debug.print aStr
	'TextBox.Text=aStr
End Sub

Sub D2(aStr)
	'debug.print aStr
	'TextBox.Text=aStr
End Sub

' ********************************
'   Attract Mode
' ********************************

Sub StartAttractMode(dummy)
	If AutoAI or AutoQA Then AutoQAStartGame.interval=13000:AutoQAStartGame.Enabled = True
	StartLightSeq
	DMDFlush
End Sub


Sub StopAttractMode
    LightSeqAttract.StopPlay
    DMDScoreNow
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 40, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 40, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqRightOn, 30, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqLeftOn, 30, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqStripe1VertOn, 50, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqSkillshot_PlayDone()
    LightSeqSkillshot.Play SeqAllOff
End Sub

Sub PlayScoopLightSeq()
		LightSeqScoopArrow.Play SeqDiagUpRightOn, 30, 1
End Sub 
Sub StopScoopLightSeq()
		LightSeqScoopArrow.StopPlay
End Sub 
Sub LightSeqScoopArrow_Playdone()
		PlayScoopLightSeq
End Sub


' ********************************
'   Lights Routines
' ********************************

Sub aRampLightsSave()
	Dim a, i
	baRampLightsSaved = True
	i = 0
	For each a in aRampLights 
		RampLightsSaveColor(i, 0) = a.name
		RampLightsSaveColor(i, 1) = a.color
		RampLightsSaveColor(i, 2) = a.colorfull
		RampLightsSaveColor(i, 3) = a.state
		RampLightsSaveColor(i, 4) = a.uservalue
		i=i+1
	next
End Sub

Sub aRampLightsRestore()
	Dim a, i
	
	if baRampLightsSaved Then
		i = 0
		for each a in aRampLights
			a.color = RampLightsSaveColor(i, 1)
			a.colorfull = RampLightsSaveColor(i, 2)
			a.state = RampLightsSaveColor(i, 3)
			a.uservalue = RampLightsSaveColor(i, 4)
			i=i+1
		Next
	End If
	baRampLightsSaved = False
End Sub

Sub aLightsSave()
	Dim a, i
	baLightsSaved = True
	i = 0
	For each a in aLights 
		LightsSaveColor(i, 0) = a.name
		LightsSaveColor(i, 1) = a.color
		LightsSaveColor(i, 2) = a.colorfull
		LightsSaveColor(i, 3) = a.state
		LightsSaveColor(i, 4) = a.uservalue
		i=i+1
	next
End Sub

Sub aLightsRestore()
	Dim a, i

	If baLightsSaved then 
		i = 0
		For each a in aLights 
			a.color = LightsSaveColor(i,1)
			a.colorfull = LightsSaveColor(i,2)
			a.state = LightsSaveColor(i,3)
			a.uservalue = LightsSaveColor(i,4)
		next
	End If 
	baLightsSaved = False
End Sub

Sub aSaveLights()
	Dim a, i
	baSaveLightsSaved = True
	i = 0
	For each a in saveLights 
		SaveColor(i, 0) = a.name
		SaveColor(i, 1) = a.color
		SaveColor(i, 2) = a.colorfull
		SaveColor(i, 3) = a.state
		SaveColor(i, 4) = a.uservalue
		i=i+1
	next
End Sub

Sub aRestoreLights()
	Dim a, i

	If baSaveLightsSaved then 
		i = 0
		For each a in saveLights 
			a.color = SaveColor(i,1)
			a.colorfull = SaveColor(i,2)
			a.state = SaveColor(i,3)
			a.uservalue = SaveColor(i,4)
		next
	End If 
	baSaveLightsSaved = False
End Sub


Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub


Sub GiOn
    DOF 126, DOFOn
    Dim bulb
    For each bulb in GI
		SetLightColor bulb, "white", -1
        bulb.State = 1
    Next
End Sub


Sub GiOff
	DOF 126, DOFOff
	Dim bulb
	For each bulb in GI
		bulb.State = 0
	Next
End Sub


Sub GiAllOn
	DOF 126, DOFOn
	Dim bulb
	For each bulb in GI
		SetLightColor bulb, "white", -1
		bulb.State = 1
	Next
End Sub

' GI & light sequence effects

Sub GiEffect(n)
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 4
            LightSeqGi.Play SeqBlinking, , 5, 100
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 5, , 1000
        Case 3 'upon
            LightSeqGi.UpdateInterval = 4
            LightSeqGi.Play SeqUpOn, 5, 1
        Case 4 ' left-right-left
            LightSeqGi.UpdateInterval = 5
            LightSeqGi.Play SeqLeftOn, 10, 1
            LightSeqGi.UpdateInterval = 5
            LightSeqGi.Play SeqRightOn, 10, 1
    End Select
End Sub


Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqBlinking, , 5, 100
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 5, , 1000
        Case 3 'upon
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 10, 1
        Case 4 ' left-right-left
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqLeftOn, 10, 1
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqRightOn, 10, 1
    End Select
End Sub

'===================
'  Flubber Flashers 
'===================	
Dim TestFlashers : TestFlashers = 0
Dim FlashLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = Table1.width : tableheight = Table1.height

'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "green" 
InitFlasher 2, "green"
InitFlasher 3, "green"
InitFlasher 4, "purple"  

InitFlasher 9, "red" : 
InitFlasher 10, "red" : 

 

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
RotateFlasher 1, 0
RotateFlasher 2, 0	
RotateFlasher 3, 90	
RotateFlasher 4, 0 ' : RotateFlasher 5,0 : RotateFlasher 6,90
'RotateFlasher 7,0 : RotateFlasher 8,0 
RotateFlasher 9,0 : RotateFlasher 10,0 
'RotateFlasher 11,0

Sub InitFlasher(nr, col) 
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 80
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	' set the texture and color of all objects
	objbase(nr).BlendDisableLighting = 1

	select Case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
		Case Else
			msgbox  "Unknown objectbase image:" & nr & " " & objbase(nr).image
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select Case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If Table1.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
'D "Called"
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 500 *  FlashLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.03 * FlashLevel(nr)^3
	objbase(nr).BlendDisableLighting =  0.5 + 10 * FlashLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * FlashLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,FlashLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	FlashLevel(nr) = FlashLevel(nr) * 0.9 - 0.01
	If FlashLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub
Sub FlasherFlash9_Timer() : FlashFlasher(9) : End Sub
Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub
Sub FlasherFlash12_Timer() : FlashFlasher(12) : End Sub


' ***      script for demoing flashers 		        ***
' *** FlashLevel(xx) = 1 : FlasherFlashxx_Timer     ***
' *** for modulated flashers use 0-1 for FlashLevel ***

Sub Timer1_Timer
 FlashLevel(10) = .6 : FlasherFlash10_Timer
 FlashLevel(11) = .6 : FlasherFlash11_Timer
 FlashLevel(12) = 1 : FlasherFlash12_Timer
 FlashLevel(9) = .6 : FlasherFlash9_Timer
 FlashLevel(1) = .6 : FlasherFlash1_Timer
 FlashLevel(2) = .9 : FlasherFlash2_Timer
 FlashLevel(3) = .9 : FlasherFlash3_Timer
 FlashLevel(4) = .6 : FlasherFlash4_Timer
end Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init
End Sub


'****************************************

Sub ResetNewBallVariables()           'reset variables for a new ball or player
	VipValue=200000
	LaneBonus = 0
End Sub


Sub ResetNewBallLights() 'turn on or off the needed lights before a new ball is released
	D "Reset elevator lights"
	SetLightColor I110,"white", 2   ' todo set this on a small delay
	SetLightColor I65, "white", 2
End Sub


Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub SmartButtonFlash(color, enabled)  'the lockbar light
	D "SmartButton Flash: " & color & " " & enabled
	If enabled Then 
		tmrSmartButtonLightFlash.Enabled = False
		If color = "green" then 						' If color is blank just use what the color was before 
			Flasherflash5_5.Color = RGB(0,255,0)
			Flasherflash5.ImageA = "domegreenflash"
			Flasherbase5.Image = "dome2basegreen"
			Flasherlit5.Image = "dome2litgreen"
		elseIf color = "white" then
			Flasherflash5_5.Color = RGB(255,255,255)
			Flasherflash5.ImageA = "domewhiteflash"
			Flasherbase5.Image = "dome2basewhite"
			Flasherlit5.Image = "dome2litwhite"
		elseIf color = "cyan" then
			Flasherflash5_5.Color = RGB(0,255,255)
			Flasherflash5.ImageA = "domeredflash"
			Flasherbase5.Image = "dome2basered"
			Flasherlit5.Image = "dome2litred"
		elseIf color = "orange" then
			Flasherflash5_5.Color = RGB(255,165,0)
			Flasherflash5.ImageA = "domeredflash"
			Flasherbase5.Image = "dome2basered"
			Flasherlit5.Image = "dome2litred"
		elseIf color = "orangedark" then
			Flasherflash5_5.Color = RGB(204,85,0)
			Flasherflash5.ImageA = "domeredflash"
			Flasherbase5.Image = "dome2basered"
			Flasherlit5.Image = "dome2litred"
		elseIf color = "red" then 
			Flasherflash5_5.Color = RGB(255,0,0)
			Flasherflash5.ImageA = "domeredflash"
			Flasherbase5.Image = "dome2basered"
			Flasherlit5.Image = "dome2litred"
		elseIf color = "blue" then 
			Flasherflash5_5.Color = RGB(0,0,255)
			Flasherflash5.ImageA = "domeblueflash"
			Flasherbase5.Image = "dome2baseblue"
			Flasherlit5.Image = "dome2litblue"
		elseIf color = "purple" then 
			Flasherflash5_5.Color = RGB(128,0,128)
			Flasherflash5.ImageA = "domepurpleflash"
			Flasherbase5.Image = "dome2basepurple"
			Flasherlit5.Image = "dome2litpurple"
		elseIf color = "yellow" then 
			Flasherflash5_5.Color = RGB(255,255,0)
			Flasherflash5.ImageA = "domeyellowflash"
			Flasherbase5.Image = "dome2baseyellow"
			Flasherlit5.Image = "dome2lityellow"
		elseIf color = "white" then 
			Flasherflash5_5.Color = RGB(255,255,255)
			Flasherflash5.ImageA = "domewhiteflash"
			Flasherbase5.Image = "dome2basewhite"
			Flasherlit5.Image = "dome2litwhite"
		End If
		tmrSmartButtonLightFlash.Enabled = True
		DOF 141, DOFOn
	elseIf SmartButtonCount <= 0 or enabled=False then 
D "TURN IT OFF!!!!!"
		tmrSmartButtonLightFlash.Enabled = False
		FlashLevel(5) = 0 : Flasherflash5_Timer
	End If
End Sub

Sub tmrSmartButtonLightFlash_Timer
	FlashLevel(5) = 1 : Flasherflash5_Timer
	If BallsOnPlayfield > 0 Then DOF 204, DOFPulse
End Sub 


Sub Flasherflash5_Timer
	dim flashx3, matdim
	If Flasherflash5.TimerEnabled = False Then 
		Flasherflash5.TimerEnabled = True
		Flasherflash5_5.TimerEnabled = True
		Flasherflash5.visible = 1
		Flasherflash5_5.visible = 1
		Flasherlit5.visible = 1
	End If
	flashx3 = FlashLevel(5) * FlashLevel(5) * FlashLevel(5)
	Flasherflash5.opacity = 200 * flashx3
	Flasherflash5_5.opacity = 500 * flashx3
	Flasherlit5.BlendDisableLighting = 10 * flashx3
	Flasherbase5.BlendDisableLighting =  flashx3
	matdim = Round(10 * FlashLevel(5))
	Flasherlit5.material = "domelit" & matdim
	FlashLevel(5) = FlashLevel(5) * 0.9 - 0.01
	If FlashLevel(5) < 0.15 Then
		Flasherlit5.visible = 0
	Else
		Flasherlit5.visible = 1
	end If
	If FlashLevel(5) < 0 Then
		Flasherflash5.TimerEnabled = False
		Flasherflash5_5.TimerEnabled = False
		Flasherflash5.visible = 0
		Flasherflash5_5.visible = 0
	End If
End Sub

Sub CheckSmartButton(bUseIt)
D "CheckSmartButton"
	Dim bUsed
	Dim a
	bUsed = False
	If smartButtonCount > 0 Then
		If bUsed and bUseIt Then  ' todo cant be used during some Modes
			'pDMDEvent(kDMD_UseSmartButton)
			SmartButtonCount = SmartButtonCount - 1
			SmartMissilePressed()
			If SmartButtonCount <= 0 Then
				SmartButtonFlash "", False
			End If
		ElseIf bUseIt = False then 	
			If SmartButtonCount <= 0 Then
				SmartButtonFlash "", False
			End If
		End If 
	End if
End Sub

'dwe3
'const kDMD_last		=521
'const kDMD_walk		=522
'const kDMD_same		=523
'const kDMD_sweet		=524
'const kDMD_dude		=525
'const kDMD_back		=526
'const kDMD_rats		=527
'const kDMD_toys		=528
'const kDMD_love		=529

Sub StartPlayerModeVideo(bSkipInitial)	' Play the GamePlay Video
	D "StartPlayerModevideo Mode:" & PlayerMode & " 2:" & PlayerMode2 & " 3:" & PlayerMode3
	Dim overVideo
	Dim bgVideo

	if bMedleyTourReady or bFinalTourReady then 
		StopPlayerModeVideo
	End If

	if tmrMedleyTour.Enabled or tmrFinalTour.Enabled then ' Wizard Mode
		playclear pBackglass
		Select Case PlayerMode3
			Case -1:' No Mode Selected  '
			Case 0: 
				pDMDEvent(kDMD_last+30)
			Case 1: 
				pDMDEvent(kDMD_walk+30)
			Case 2: 
				pDMDEvent(kDMD_same+30)
			Case 3: 
				pDMDEvent(kDMD_sweet+30)
			Case 4: 
				pDMDEvent(kDMD_dude+30)
			Case 5: 
				pDMDEvent(kDMD_back+30)
			Case 6: 
				pDMDEvent(kDMD_rats+30)
			Case 7: 
				pDMDEvent(kDMD_toys+30)
			Case 8: 
				pDMDEvent(kDMD_love+30)
		End Select
	elseif bSecondMode = False Then
		playclear pBackglass
		If PlayerMode <> -1 Then 
			If ModePercent(PlayerMode) < 0 then ModePercent(PlayerMode)=0
		End If
		Select Case PlayerMode
			Case -1:' No Mode Selected  '
			Case 0: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_last+10)
				End If 
				pDMDEvent(kDMD_last+30)
			Case 1: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_walk+10)
				End If 
				pDMDEvent(kDMD_walk+30)
			Case 2: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_same+10)
				End If 
				pDMDEvent(kDMD_same+30)
			Case 3: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_sweet+10)
				End If 
				pDMDEvent(kDMD_sweet+30)
			Case 4: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_dude+10)
				End If 
				pDMDEvent(kDMD_dude+30)
			Case 5: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_back+10)
				End If 
				pDMDEvent(kDMD_back+30)
			Case 6: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_rats+10)
				End If 
				pDMDEvent(kDMD_rats+30)
			Case 7: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_toys+10)
				End If 
				pDMDEvent(kDMD_toys+30)
			Case 8: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_love+10)
				End If 
				pDMDEvent(kDMD_love+30)   ' 529+30
		End Select
	ElseIf PlayerMode2 <> -1 Then
		playclear pBackglass
		Select Case PlayerMode2
			Case -1:
			Case 0:
				overVideo = "Loops-Video-0x0059.mp4"
				bgvideo = "Video-0x0000.mp4"
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x059C-superloops.wav",100, 1
			Case 1:
				overVideo = "Ramps-Video-0x005B.mp4"
				bgvideo = "Video-0x0000.mp4"
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x0346-superramps.wav",100, 1
			Case 2:
				overVideo = "Targets-Video-0x005E.mp4"
				bgvideo = "Video-0x0002.mp4"
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x053Bsupertargets.wav",100, 1
			Case 3:
				overVideo = "Scoring-Video-0x005C.mp4"
				bgvideo = "Video-0x0000.mp4"
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x0315doublescoring.wav",100, 1
			Case 4:
				overVideo = "Lanes-Video-0x0058.mp4"
				bgvideo = "Video-0x0000.mp4"
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x0598-superlanes.wav",100, 1
			Case 5:
				overVideo = "Pops-Video-0x005A.mp4"
				bgvideo = "Video-0x0002.mp4"
				'PlaySoundVol "Sound-0x0598-superlanes", VolDef
			Case 6:
				overVideo = "Spinners-Video-0x005D.mp4"
				bgvideo = "Video-0x0051.mp4"
				'PlaySoundVol "Sound-0x0598-superlanes", VolDef
		End Select
		If PlayerMode2 <> -1 then ' Just make sure  'TODO Put this to ADDScene so we block the Big Numbers
'			QueueScene "playmedia """&overVideo&""", ""PupVideos"", pOverVid , """", -1, """", 1, 1 '", 3540, 2	
			QueueScene "ScenePlayMessage """&overVideo&""", """","""","""" '", 3500, 2
			QueueScene "SceneClearPlayMessage '", 0, 2

'example		QueueScene "ScenePlayMessage ""Video-0x0006.mp4"", """",""SKILL SHOT"","""" '", 2000, 1
'				QueueScene "SceneClearPlayMessage '", 0, 1
	
			playmedia bgVideo, "PupVideos", pBackglass, "", -1, "", 1, 2
		End If 
	End If
End Sub


'************ PuPDMD
Sub pDMDStartGame
	Dim fsize
	fsize=8*FontScale
	If bUsePUPDMD then 
		D "pDMDStartGame"
		pInAttract=false
		If pDMDVideoPlaying Then 
			PuPlayer.playstop pDMD
			pDMDVideoPlaying=False
		End if
		' Clear the overvideo just in Case it is playing 
		playclear pOverVid
		PuPlayer.LabelSet pOverVid,"OverMessage1"," ",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2"," ",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage3"," ",1,"{'mt':2, 'size': "&fsize&",'ypos': 65}"
		pDMDSetPage(pScores)   'set blank text overlay page.
	end If
End Sub

Sub pDMDGameOver
	D "pDMDGameOver"	
	DisplayDMDText2 "_", "GAME OVER", 20000, 5, 0
	StartAttractMode 1

	If bUsePUPDMD then
		PuPlayer.PlayStop pOverVid
		PuPlayer.SetLoop pOverVid, 0

		playclear pBackglass
		playclear pMusic
		playclear pAudio

		If bUsePUPDMD then 
			PuPlayer.LabelShowPage pOverVid, 1,0,""
			PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x0047.mp4", 1, 1
			PuPlayer.LabelSet pOverVid,"OverMessage1","GAME" ,1,"{'mt':2, 'size': 14 }"
			PuPlayer.LabelSet pOverVid,"OverMessage2","",1,""
			PuPlayer.LabelSet pOverVid,"OverMessage3","OVER",1,"{'mt':2, 'size': 14 }"
		End If 
	End If 
	pAttractStart
End Sub


Sub pAttractStart
	If (pInAttract = False) then 
		D "pAttractStart"
		pDMDSetPage(pDMDBlank)   'set blank text overlay page.
		pCurAttractPos=0
		pDMDStartUP
	end If 
End Sub


Sub pDMDEvent(id)
	If bUsePUPDMD then
		D2 "pDMDEvent " & id 
		PauseBigScore=True
		PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""
		PuPEvent(id)  ' Send an event to the pup pack the E500 trigger
	End If 
End Sub

'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
SpinnerSoundLevel = 0.5/5												'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero


'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
    PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
    PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundVol(soundname, Volume)
  PlaySound soundname, 1, Volume
End Sub


' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / tableheight-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
    PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
    RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
    RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
	End Select
End Sub

Sub SoundNudgeRight()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
	End Select
End Sub

Sub SoundNudgeCenter()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
	End Select
End Sub


Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger	
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub


'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, Plunger
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////
Sub RandomSoundDrain(drainswitch)
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic ("Drain_1"), DrainSoundLevel, drainswitch
		Case 2 : PlaySoundAtLevelStatic ("Drain_2"), DrainSoundLevel, drainswitch
		Case 3 : PlaySoundAtLevelStatic ("Drain_3"), DrainSoundLevel, drainswitch
		Case 4 : PlaySoundAtLevelStatic ("Drain_4"), DrainSoundLevel, drainswitch
		Case 5 : PlaySoundAtLevelStatic ("Drain_5"), DrainSoundLevel, drainswitch
		Case 6 : PlaySoundAtLevelStatic ("Drain_6"), DrainSoundLevel, drainswitch
		Case 7 : PlaySoundAtLevelStatic ("Drain_7"), DrainSoundLevel, drainswitch
		Case 8 : PlaySoundAtLevelStatic ("Drain_8"), DrainSoundLevel, drainswitch
		Case 9 : PlaySoundAtLevelStatic ("Drain_9"), DrainSoundLevel, drainswitch
		Case 10 : PlaySoundAtLevelStatic ("Drain_10"), DrainSoundLevel, drainswitch
		Case 11 : PlaySoundAtLevelStatic ("Drain_11"), DrainSoundLevel, drainswitch
	End Select
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBallRelease(drainswitch)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("BallRelease1",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 2 : PlaySoundAtLevelStatic SoundFX("BallRelease2",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 3 : PlaySoundAtLevelStatic SoundFX("BallRelease3",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 4 : PlaySoundAtLevelStatic SoundFX("BallRelease4",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 5 : PlaySoundAtLevelStatic SoundFX("BallRelease5",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 6 : PlaySoundAtLevelStatic SoundFX("BallRelease6",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 7 : PlaySoundAtLevelStatic SoundFX("BallRelease7",DOFContactors), BallReleaseSoundLevel, drainswitch
	End Select
End Sub



'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_L1",DOFContactors), SlingshotSoundLevel, Sling
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_L2",DOFContactors), SlingshotSoundLevel, Sling
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_L3",DOFContactors), SlingshotSoundLevel, Sling
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_L4",DOFContactors), SlingshotSoundLevel, Sling
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_L5",DOFContactors), SlingshotSoundLevel, Sling
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_L6",DOFContactors), SlingshotSoundLevel, Sling
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_L7",DOFContactors), SlingshotSoundLevel, Sling
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_L8",DOFContactors), SlingshotSoundLevel, Sling
		Case 9 : PlaySoundAtLevelStatic SoundFX("Sling_L9",DOFContactors), SlingshotSoundLevel, Sling
		Case 10 : PlaySoundAtLevelStatic SoundFX("Sling_L10",DOFContactors), SlingshotSoundLevel, Sling
	End Select
End Sub

Sub RandomSoundSlingshotRight(sling)
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_R1",DOFContactors), SlingshotSoundLevel, Sling
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_R2",DOFContactors), SlingshotSoundLevel, Sling
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_R3",DOFContactors), SlingshotSoundLevel, Sling
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_R4",DOFContactors), SlingshotSoundLevel, Sling
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_R5",DOFContactors), SlingshotSoundLevel, Sling
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_R6",DOFContactors), SlingshotSoundLevel, Sling
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_R7",DOFContactors), SlingshotSoundLevel, Sling
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_R8",DOFContactors), SlingshotSoundLevel, Sling
	End Select
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
End Sub

Sub RandomSoundBumperMiddle(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
End Sub

Sub RandomSoundBumperBottom(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
		PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_L01",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_L02",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_L07",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_L08",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_L09",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_L10",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_L12",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_L14",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_L18",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_L20",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_L26",DOFFlippers), FlipperLeftHitParm, Flipper
	End Select
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_R01",DOFFlippers), FlipperRightHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_R02",DOFFlippers), FlipperRightHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_R03",DOFFlippers), FlipperRightHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_R04",DOFFlippers), FlipperRightHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_R05",DOFFlippers), FlipperRightHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_R06",DOFFlippers), FlipperRightHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_R07",DOFFlippers), FlipperRightHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_R08",DOFFlippers), FlipperRightHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_R09",DOFFlippers), FlipperRightHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_R10",DOFFlippers), FlipperRightHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_R11",DOFFlippers), FlipperRightHitParm, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpRight(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_8",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
 	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_1"), parm  * RubberFlipperSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_2"), parm  * RubberFlipperSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_3"), parm  * RubberFlipperSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_4"), parm  * RubberFlipperSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_5"), parm  * RubberFlipperSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_6"), parm  * RubberFlipperSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_7"), parm  * RubberFlipperSoundFactor
	End Select
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rollover_1"), RolloverSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("Rollover_2"), RolloverSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("Rollover_3"), RolloverSoundLevel
		Case 4 : PlaySoundAtLevelActiveBall ("Rollover_4"), RolloverSoundLevel
	End Select
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 5 then		
 		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
 		RandomSoundRubberWeak()
 	End If	
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_1"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_2"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_5"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_6"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_7"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_8"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_9"), Vol(ActiveBall) * RubberWeakSoundFactor
	End Select
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 5 then
 		RandomSoundRubberStrong 1 
	End if
	If finalspeed <= 5 then
 		RandomSoundRubberWeak()
 	End If	
End Sub

Sub RandomSoundWall()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
 	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
	Select Case Int(Rnd*13)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Metal_Touch_1"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Metal_Touch_2"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Metal_Touch_3"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Metal_Touch_4"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Metal_Touch_5"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Metal_Touch_6"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Metal_Touch_7"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Metal_Touch_8"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Metal_Touch_9"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 10 : PlaySoundAtLevelActiveBall ("Metal_Touch_10"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 11 : PlaySoundAtLevelActiveBall ("Metal_Touch_11"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 12 : PlaySoundAtLevelActiveBall ("Metal_Touch_12"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 13 : PlaySoundAtLevelActiveBall ("Metal_Touch_13"), Vol(ActiveBall) * MetalImpactSoundFactor
	End Select
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_2"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
 	End If
	If finalspeed < 6 Then
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_1"), BottomArchBallGuideSoundFactor * 0.25
		Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_2"), BottomArchBallGuideSoundFactor * 0.25
		Case 3 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_3"), BottomArchBallGuideSoundFactor * 0.25
	End Select
End Sub


Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Medium_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
 	End If
	If finalspeed < 6 Then
 		Select Case Int(Rnd*7)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Soft_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Soft_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Soft_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 4 : PlaySoundAtLevelActiveBall ("Apron_Soft_4"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 5 : PlaySoundAtLevelActiveBall ("Apron_Soft_5"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 6 : PlaySoundAtLevelActiveBall ("Apron_Soft_6"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 7 : PlaySoundAtLevelActiveBall ("Apron_Soft_7"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  SPINNER  ////////////////////////////
Sub SoundSpinner()
	PlaySoundAtLevelStatic ("Spinner_12"), SpinnerSoundLevel, sw38
End Sub


'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_5",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_6",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_7",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_8",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor		
	End Select
End Sub

Sub RandomSoundTargetHitWeak()
	Select Case Int(Rnd*4)+1		
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_1",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_2",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_3",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_4",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
	End Select
End Sub

Sub PlayTargetSound()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 10 then
 		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft Activeball
	Else 
 		RandomSoundTargetHitWeak()
 	End If	
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound	
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_3"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_4"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_6"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
	End Select
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	Select Case Int(Rnd*2)+1				
		Case 1 : PlaySoundAtLevelStatic ("Gate_FastTrigger_1"), GateSoundLevel, Activeball
		Case 2 : PlaySoundAtLevelStatic ("Gate_FastTrigger_2"), GateSoundLevel, Activeball
	End Select
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)	
	SoundPlayfieldGate	
End Sub	

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_L1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_L2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_L3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_L4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
End Sub

Sub RandomSoundRightArch()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_R1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_R2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_R3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_R4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
End Sub


Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.velx < -8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If Activeball.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If activeball.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	Select Case Int(Rnd*2)+1	
		Case 1: PlaySoundAtLevelStatic ("Saucer_Enter_1"), SaucerLockSoundLevel, Activeball
		Case 2: PlaySoundAtLevelStatic ("Saucer_Enter_2"), SaucerLockSoundLevel, Activeball
	End Select
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select

	PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'******************************************************
'		TRACK ALL BALL VELOCITIES
' 		FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			If b.id >= HighestID then highestID = b.id
		Next

		If uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		If uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		If uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

Sub RDampen_Timer()
	Cor.Update
End Sub

' ***************************************************************************************************
' ******************************  Lampz from funhouse
Dim MaterialWhiteArray: MaterialWhiteArray = Array("BulbWhiteOff", "BulbWhiteOff","BulbWhiteOff","BulbWhiteOn")
Dim MaterialBlueArray: MaterialBlueArray = Array("BulbBlueOff", "BulbBlueOff","BulbBlueOff","BulbBlueOn")
Dim MaterialRedArray: MaterialRedArray = Array("BulbRedOff", "BulbRedOff","BulbRedOff","BulbRedOn")
Dim MaterialYellowArray: MaterialYellowArray = Array("BulbYellowOff", "BulbYellowOff","BulbYellowOff","BulbYellowOn")

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              ' Setup lamp assignments
LampTimer.Interval = -1
LampTimer.Enabled = 1

Sub LampTimer_Timer()
	dim x, chglamp
	If b2son then chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update1	'update (fading logic only)
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
LampTimer2.Interval = -1
LampTimer2.Enabled = True
Sub LampTimer2_Timer()	'Stealing this random wall's timer for -1 updates
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

'***Material Swap***
'Fade material for green, red, yellow colored Bulb prims
Sub FadeMaterialColoredBulb(pri, group, ByVal aLvl)	'cp's script
	If Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select Case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
        Case 3:pri.Material = group(3) 'Full
    End Select
	pri.blenddisablelighting = aLvl * 1 'Intensity Adjustment
End Sub


'Fade material for red, yellow colored bulb Filiment prims
Sub FadeMaterialColoredFiliment(pri, group, ByVal aLvl)	'cp's script
	If Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select Case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
        Case 3:pri.Material = group(3) 'Full
    End Select
	'If tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 50  'Intensity Adjustment
End Sub


Sub InitLampsNF()
	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 3/10 : Lampz.FadeSpeedDown(x) = 3/10 : next

	'Lamp Assignments
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
    Lampz.MassAssign(1)= l39
    Lampz.Callback(1) =         "FadeMaterialColoredBulb pFBase39, MaterialBlueArray, "
    Lampz.Callback(1) = "FadeMaterialColoredFiliment pFiliment39, MaterialBlueArray, "

    Lampz.MassAssign(2)= l40
    Lampz.Callback(2) =         "FadeMaterialColoredBulb pFBase40, MaterialYellowArray, "
    Lampz.Callback(2) = "FadeMaterialColoredFiliment pFiliment40, MaterialYellowArray, "

	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.update

End Sub


'====================
'Class jungle nf
'=============

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors If empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: If using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks

Class LampFader
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Public UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)
	Private Mult(140)
	Public FrameTime
	Private InitFrame
	Public Name

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			If IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		'debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : If UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next
		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'If idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		If Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'If empty, use Set
			If IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			If IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					If typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				If typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			end if
		Next
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			If not Lock(x) then 'and not Loaded(x) then
				If OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					If Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseIf Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					If Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			If not Lock(x) then 'and not Loaded(x) then
				If OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					If Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseIf Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					If Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			If not Loaded(x) then
				If IsArray(obj(x) ) Then	'If array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'If single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				If TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox  "uhh " & 2 & " = " & lvl(x)
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					If Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class

'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl^1.6	'exponential curve?
End Function


'Helper functions
Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String)
	P Callback
	If err.number = 13 then  msgbox  "Proc error! No such procedure: " & vbnewline & string
	If err.number = 424 then msgbox  "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	If IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'If not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				If isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
		AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'If not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			If isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function

' ******************************  END Lampz from funhouse
' ***************************************************************************************************


' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in Case it is needed later
' *********************************************************************

' Slingshots has been hit

Dim LStep, RStep

Sub RightSlingShot_Slingshot
	RandomSoundSlingshotRight SLING1
	FlashLevel(9) = 1 : FlasherFlash9_Timer
	RSling.Visible = 0
	RSling1.Visible = 1
	sling1.TransZ = -20
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	AddScore 30
End Sub


Sub RightSlingShot_Timer
	Select Case RStep
		Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
		Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	RandomSoundSlingshotLeft SLING2
	FlashLevel(10) = 1 : FlasherFlash10_Timer
	LSling.Visible = 0
	LSling1.Visible = 1
	sling2.TransZ = -20
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	AddScore 30
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
		Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

Sub tmrPops_Timer
	tmrPops.Enabled=False
	PopScore=PopScoreCurrent
	PopScoreCurrent=0
End Sub

Sub Bumper001_Hit
    If Tilted Then Exit Sub
	RandomSoundBumperTop Bumper001

	'BumperLight001.State = 2
	FlashForMs BumperLight001, 200, 50, 0

	'FlashForMs Bumper1Light, 200, 50, 0

	If PlayerMode=5 and not bModeProgressUpgraded and StackState(kStack_Pri0).GetArrowState(I107) = 0 then 'move the shot
		If StackState(kStack_Pri0).GetArrowState(I35) <> 0 Then
			SSetLightColor kStack_Pri0, I35, modeRampColor, 0
			SSetLightColor kStack_Pri0, I62, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I62) <> 0 Then
			SSetLightColor kStack_Pri0, I62, modeRampColor, 0
			SSetLightColor kStack_Pri0, I81, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I81) <> 0 Then
			SSetLightColor kStack_Pri0, I81, modeRampColor, 0
			SSetLightColor kStack_Pri0, I86, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I86) <> 0 Then
			SSetLightColor kStack_Pri0, I86, modeRampColor, 0
			SSetLightColor kStack_Pri0, I91, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I91) <> 0 Then
			SSetLightColor kStack_Pri0, I91, modeRampColor, 0
			SSetLightColor kStack_Pri0, I97, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I97) <> 0 Then
			SSetLightColor kStack_Pri0, I97, modeRampColor, 0
			SSetLightColor kStack_Pri0, I103, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I103) <> 0 Then
			SSetLightColor kStack_Pri0, I103, modeRampColor, 0
			SSetLightColor kStack_Pri0, I35, modeRampColor, 1
		End If
	End if

	If Not bWizardMode Then
		PopHits=PopHits-1
		BumperBonus=BumperBonus+1
		If PopHits <=0 Then
			PopValue=int(PopValue*1.15)
			BumperMultiplier=BumperMultiplier+1
			PopHits=25
			PopLevel=PopLevel+1
			SetPopColors()
		End If
	End If

	AddScore (PopValue * BumperMultiplier)
	PopScoreCurrent=PopScoreCurrent+(PopValue * BumperMultiplier)
	tmrPops.Enabled=False
	tmrPops.Enabled=True
    CheckModeProgress("switch")
	CheckModeProgress("bumper")
	LastSwitchHit="bumper"
End Sub

Sub Bumper002_Hit
    If Tilted Then Exit Sub
	RandomSoundBumperMiddle Bumper002

	FlashForMs BumperLight002, 200, 50, 0
	'BumperLight002.State = 2
    'FlashForMs Bumper2Light, 200, 50, 0

	If PlayerMode=5 and not bModeProgressUpgraded and StackState(kStack_Pri0).GetArrowState(I107) = 0 then 'move the shot
		If StackState(kStack_Pri0).GetArrowState(I35) <> 0 Then
			SSetLightColor kStack_Pri0, I35, modeRampColor, 0
			SSetLightColor kStack_Pri0, I62, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I62) <> 0 Then
			SSetLightColor kStack_Pri0, I62, modeRampColor, 0
			SSetLightColor kStack_Pri0, I81, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I81) <> 0 Then
			SSetLightColor kStack_Pri0, I81, modeRampColor, 0
			SSetLightColor kStack_Pri0, I86, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I86) <> 0 Then
			SSetLightColor kStack_Pri0, I86, modeRampColor, 0
			SSetLightColor kStack_Pri0, I91, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I91) <> 0 Then
			SSetLightColor kStack_Pri0, I91, modeRampColor, 0
			SSetLightColor kStack_Pri0, I97, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I97) <> 0 Then
			SSetLightColor kStack_Pri0, I97, modeRampColor, 0
			SSetLightColor kStack_Pri0, I103, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I103) <> 0 Then
			SSetLightColor kStack_Pri0, I103, modeRampColor, 0
			SSetLightColor kStack_Pri0, I35, modeRampColor, 1
		End If
	End if

	If Not bWizardMode Then
		PopHits=PopHits-1
		BumperBonus=BumperBonus+1
		If PopHits <=0 Then
			PopValue=int(PopValue*1.15)
			BumperMultiplier=BumperMultiplier+1
			PopHits=25
			PopLevel=PopLevel+1
			SetPopColors()
		End If
	End If

	AddScore (PopValue * BumperMultiplier)
	PopScoreCurrent=PopScoreCurrent+(PopValue * BumperMultiplier)
	tmrPops.Enabled=False
	tmrPops.Enabled=True
    CheckModeProgress("switch")
	CheckModeProgress("bumper")
	LastSwitchHit="bumper"
End Sub

Sub Bumper003_Hit
    If Tilted Then Exit Sub
	RandomSoundBumperBottom Bumper003

	FlashForMs BumperLight003, 200, 50, 0
	'BumperLight003.State = 2
    'FlashForMs Bumper3Light, 200, 50, 0

	If PlayerMode=5 and not bModeProgressUpgraded and StackState(kStack_Pri0).GetArrowState(I107) = 0 then 'move the shot
		If StackState(kStack_Pri0).GetArrowState(I35) <> 0 Then
			SSetLightColor kStack_Pri0, I35, modeRampColor, 0
			SSetLightColor kStack_Pri0, I62, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I62) <> 0 Then
			SSetLightColor kStack_Pri0, I62, modeRampColor, 0
			SSetLightColor kStack_Pri0, I81, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I81) <> 0 Then
			SSetLightColor kStack_Pri0, I81, modeRampColor, 0
			SSetLightColor kStack_Pri0, I86, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I86) <> 0 Then
			SSetLightColor kStack_Pri0, I86, modeRampColor, 0
			SSetLightColor kStack_Pri0, I91, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I91) <> 0 Then
			SSetLightColor kStack_Pri0, I91, modeRampColor, 0
			SSetLightColor kStack_Pri0, I97, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I97) <> 0 Then
			SSetLightColor kStack_Pri0, I97, modeRampColor, 0
			SSetLightColor kStack_Pri0, I103, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I103) <> 0 Then
			SSetLightColor kStack_Pri0, I103, modeRampColor, 0
			SSetLightColor kStack_Pri0, I35, modeRampColor, 1
		End If
	End if

	If Not bWizardMode Then
		PopHits=PopHits-1
		BumperBonus=BumperBonus+1
		If PopHits <=0 Then
			PopValue=int(PopValue*1.15)
			BumperMultiplier=BumperMultiplier+1
			PopHits=25
			PopLevel=PopLevel+1
			SetPopColors()
		End If
	End If

	AddScore (PopValue * BumperMultiplier)
	PopScoreCurrent=PopScoreCurrent+(PopValue * BumperMultiplier)
	tmrPops.Enabled=False
	tmrPops.Enabled=True
    CheckModeProgress("switch")
	CheckModeProgress("bumper")
	LastSwitchHit="bumper"
End Sub

Sub SetPopColors()
	Select Case PopLevel:
		Case 0: SetLightColor BumperLight001, "white", 0:SetLightColor BumperLight002, "white", 0:SetLightColor BumperLight003, "white", 0
		Case 3: SetLightColor BumperLight001, "cyan", 0:SetLightColor BumperLight002, "cyan", 0:SetLightColor BumperLight003, "cyan", 0
		Case 5: SetLightColor BumperLight001, "red", 0:SetLightColor BumperLight002, "red", 0:SetLightColor BumperLight003, "red", 0
		Case 8: SetLightColor BumperLight001, "blue", 0:SetLightColor BumperLight002, "blue", 0:SetLightColor BumperLight003, "blue", 0
		Case 12: SetLightColor BumperLight001, "purple", 0:SetLightColor BumperLight002, "purple", 0:SetLightColor BumperLight003, "purple", 0
		Case 14: SetLightColor BumperLight001, "yellow", 0:SetLightColor BumperLight002, "yellow", 0:SetLightColor BumperLight003, "yellow", 0
		Case 16: SetLightColor BumperLight001, "green", 0:SetLightColor BumperLight002, "green", 0:SetLightColor BumperLight003, "green", 0
	End Select
End Sub

Sub sw61_hit 'saucer
	Dim delay, ShootToyBox
	SoundSaucerLock
 	delay=2000
	shootToyBox=false
	If BallsOnPlayfield = 1 then bPauseTimer=True

	If tmrSkillshot.Enabled and LastSwitchHit="ballsavestarttrigger" Then		
		D "SCORE SKILLSHOT"
		AddScore 100000
		DOF 127, DOFPulse

		QueueScene "ScenePlayMessage ""Video-0x0006.mp4"", """",""SKILL SHOT"","""" '", 2000, 1
		QueueScene "SceneClearPlayMessage '", 0, 1
		SmartButtonCount=SmartButtonCount+1
		'todo add to timer
	Else
		AddScore 1200
	End If 
	'TODO Add some timer time
	pJesterhead.Collidable=False
	CheckModeProgress("I97")
	Delay=2200
	If I100.state=2 and not bElevMultiBall Then ' lock is lit
		SetLightColor I100, "green", 0
		bStartMB=False
		If I75.state=2 Then
				shootToyBox=True:I75.state=1
				If bToyBoxMultiball Then ' todo add 2x timer
					D "ToyBoxMB 2x earned"
					QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", ""500,000"",""BALL LOCKED"",""ALL SCORES 2X FOR 20 SECONDS"" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
					bToyBoxBonus3x = False
D "Starting 2x"
					tmrToyBoxMBBonus.Enabled = True
					tmrToyBoxMBBonus.UserValue = 20
					AddScore 500000
					AddPlayMultiplier 2
					Delay=500
				Else
					QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 TOY"",""LOCKED"","""" '", 1000, 1
					QueueScene "SceneClearPlayMessage '", 0, 1
				End If
		ElseIf I76.state=2 Then	
				shootToyBox=True:I76.state=1
				If bToyBoxMultiball Then ' todo add 3x timer
					QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", ""550,000"",""BALL LOCKED"",""ALL SCORES 3X FOR 20 SECONDS"" '", 1500, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
					AddScore 550000
					If tmrToyBoxMBBonus.Enabled then
						tmrToyBoxMBBonus.Enabled = True
						tmrToyBoxMBBonus.UserValue = 20
						If bToyBoxBonus3x=False then 
							bToyBoxBonus3x = True
							AddPlayMultiplier 3/2					' Remove the 2x and add the 3x
						Else
							AddPlayMultiplier 2
						End If
D "Starting 3x"
						Delay=500
					end if
				Else
					QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""2 TOYS"",""LOCKED"","""" '", 1000, 1
					QueueScene "SceneClearPlayMessage '", 0, 1
				End If   'TODO DONT ALLOW ANYMORE LOCKS with 3x running.
		ElseIf I77.state=2 Then		
				shootToyBox=True:I77.state=1  'todo start multiball - allow to cancel
				bStartMB=True
				vpmTimer.addtimer 7500,"ToyBoxMB_Start '"
				ScenePlayMessage "Video-0x0044.mp4", "","",""
		ElseIf I78.state=2 Then		
				shootToyBox=True:I78.state=1
				bStartMB=True
				vpmTimer.addtimer 7500,"ToyBoxMB_Start '"
				ScenePlayMessage "Video-0x0044.mp4", "","",""
		ElseIf I79.state=2 Then		
				shootToyBox=True:I79.state=1
				bStartMB=True
				vpmTimer.addtimer 7500,"ToyBoxMB_Start '"
				ScenePlayMessage "Video-0x0044.mp4", "","",""
		ElseIf I80.state=2 Then		
				shootToyBox=True:I80.state=1
				bStartMB=True
				vpmTimer.addtimer 7500,"ToyBoxMB_Start '"
				ScenePlayMessage "Video-0x0044.mp4", "","","" 
			End If
	End If
	If BallsOnPlayfield = 1 then ' only 1 ball left, dont let it be locked
		If bToyBoxMultiball Then
			SetLightColor I100, "green",0
		end if
'todo check If timer is active
'pause timer
'addtimer to restart timer
	End If
	vpmTimer.addtimer Delay, "RightEject """ & shootToyBox & """ '"
End Sub

dim bBallInBox, bStartMB
Sub RightEject(ShootToyBox)
	D "Right Eject " & ShootToyBox
	If ShootToyBox then
		SoundSaucerKick 1, sw61
		sw61.Kick -36, 37, 55 
		bBallInBox=False
		vpmTimer.addtimer 2000, "ToyBox_CloseLid '"
		vpmTimer.addtimer 2000, "ToyBox_Check '"
' check for success
' If success then release ball in shooter Lane
' either Case
' close lid
	Else
		SoundSaucerKick 1, sw61
		sw61.Kick -36, 7+RndNum(0,5)
		AddScore 1500
		bPauseTimer=False
	End If
' Relight LOCK If earned
	If not bElevMultiBall and not bWizardMode then
		If I80.state=2 or I79.state=2 or I78.state=2 or I77.state=2 or I76.state=2 Then
			D "Relight LOCK light"
			SetLightColor I100, "green", 2
		End If
	End If
End Sub

Sub ToyBoxKicker_Hit
	bBallInBox=True
	RandomSoundDrain ToyBoxKicker
	D "Ball in BOX"

	RealBallsInLock=RealBallsInLock+1
	BallsOnPlayfield=BallsOnPlayfield-1
	I "TOYBOX KICKER Add BOP: " & ballsOnPlayfield & " RealBall: " & RealBallsInLock

	ToyBoxKicker.DestroyBall
	'ToyBoxLocked()
End Sub

Dim SaveI65, SaveI94, SaveI110

Sub ToyBoxMB_Start
	Dim ballsLocked,a,b 
	If bStartMB Then
		bToyBoxMultiball=True
		ToyBoxMBAttempts=ToyBoxMBAttempts+1
		QueueScene "ScenePlayMessage ""Video-0x0065.mp4"", ""TOY BOX"",""MULTIBALL"","""" '", 7500, 1
		QueueScene "SceneClearPlayMessage '", 0, 1

		' determine number of balls to release, release the realballs first up to the number of balls in locks

		bToyBoxMultiBall=True
		ToyBoxMultiBallCount=0
		SetLightColor I22, "green", 2		' Flash ToyBox on lower playfield

		SetLightColor F147,"white", 2

' Turn off Elevator Lights and Lock Lights
		SaveI65=I65.state
		SaveI94=I94.state
		SaveI110=I110.state
		SetLightColor I94, "white", 0
		SetLightColor I65, "white", 0
		SetLightColor I110,"white", 0

		PuPlayer.playlistplayex pMusic,"audioclear","clear.mp3",100, 1
	
		SaveMode=PlayerMode
		PlayerMode=7
		StartPlayerModeVideo False
		PlaySong "Song-" & PlayerMode & ".mp3"
		PlayerMode=SaveMode

		StackState(kStack_Pri1).Enable(8)
		b=0
		For each a in aRampLights
			SSetLightColor kStack_Pri1, a, "green", 2
			shots(b)=2   ' takes 2 shots to turn it off
			b=b+1
		Next
		D "Lights and about to start mb"
		ToyBoxMBJackpotBase=500000
		BallsLocked=3
		If I78.state=1 then BallsLocked=4:ToyBoxMBJackpotBase=600000
		If I79.state=1 then BallsLocked=5:ToyBoxMBJackpotBase=700000
		If I80.state=1 then BallsLocked=6:ToyBoxMBJackpotBase=1000000

		for each b in aLockLights
			SetLightColor b, "green", 0
			b.uservalue=2  ' todo higher each time
		Next
		SetLightColor F147, "white", 2

		D "ToyBoxMB: BallsLocked=" & BallsLocked & " Real Ballslocked=" & RealBallsInLock
		If RealBallsInLock > 0 Then
			If RealBallsInLock <= ballsLocked then
				BallsLocked=BallsLocked-RealBallsInLock
				I "ToyBoxMB Adding Real balls " & RealBallsInLock
				AddMultiball RealBallsInLock
				RealBallsInLock=0
				I "TOYBOX MB Add BOP: " & ballsOnPlayfield & " RealBall: " & RealBallsInLock
			Else
				I "ToyBoxMB Adding ALL Real balls " & ballsLocked
				RealBallsInLock=RealBallsInLock-ballsLocked
				AddMultiball ballsLocked
				ballsLocked=0
				I "TOYBOX2 MB Add BOP: " & ballsOnPlayfield & " RealBall: " & RealBallsInLock
			End If
		End if
		If BallsLocked > 0 then 
			I "Virtual Balls " & ballsLocked
			AddMultiball ballsLocked
		End if
		I "ToyBoxMB: After ...  " & ballsLocked & " Real:" & RealBallsInLock
	Else
		D "ToyBoxMB - Cancelled"
	End If
End Sub

Sub ToyBox_Cancel()
	D "ToyBox_Cancel " & bStartMB    ' last solid light is the last ball locked
	If I77.state=1 and I78.state<>1 Then
		D "ToyBox Cancel #3"
		I77.state=1:bStartMB=False
		QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""3 TOYS"",""LOCKED"","""" '", 1000, 1
		QueueScene "SceneClearPlayMessage '", 0, 1
	ElseIf I78.state=1 and I79.state<>1 Then		
		D "ToyBox Cancel #4"
		I78.state=1:bStartMB=False
		QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""4 TOYS"",""LOCKED"","""" '", 1000, 1
		QueueScene "SceneClearPlayMessage '", 0, 1
	ElseIf I79.state=1 and I80.state<>1 Then
		D "ToyBox Cancel #5"		
		I79.state=1:bStartMB=False
		QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""5 TOYS"",""LOCKED"","""" '", 1000, 1
		QueueScene "SceneClearPlayMessage '", 0, 1
	End If
	If I78.state=2 or I79.state=2 or I80.state=2 Then
		SetLightColor I100, "green", 2
	End If
	D "ToyBox_Cancel ..Done." & bStartMB
End Sub

Sub ToyBox_CloseLid
	D "===CLOSE LID=="
End Sub

Sub ToyBox_Check
	Dim a
	D "Check If successful toybox shot"
	If bBallInBox and not bStartMB and not bToyBoxMultiball Then
		I "Ball locked in toybox successfully StartMB?" & bStartMB
		AddMultiball 1
		' bPauseTimer handled in the plunger lane
	Else	
		bPauseTimer=False
	End If
	If bToyBoxMultiball then ' you locked a ball during toybox MB_Multiplier
'If no playfield balls left then release the balls in the lock
		If BallsOnPlayfield=0 Then
			I "BallsOnPlayfield=0 - add 1"
			AddMultiball RealBallsInLock
			RealBallsInLock=0
			I "TOYBOX EMPTY: " & ballsOnPlayfield & " RealBall: " & RealBallsInLock
			for each a in aLockLights
				SetLightColor a, "green", 0
				a.uservalue=2 ' todo higher each time
			Next
		End If
	end if
	bBallInBox=False
End Sub

Sub tmrToyBoxMBBonus_Timer()
	Dim a
	D "ToyBoxMB Timer " & tmrToyBoxMBBonus.UserValue

	tmrToyBoxMBBonus.UserValue = tmrToyBoxMBBonus.UserValue - 1
	If bToyBoxBonus3x then 
		If bUsePUPDMD then PuPlayer.LabelSet pBackglass,"MTimer",	"3X FOR " & tmrToyBoxMBBonus.UserValue	,1,""
	Else
		If bUsePUPDMD then PuPlayer.LabelSet pBackglass,"MTimer",	"2X FOR " & tmrToyBoxMBBonus.UserValue	,1,""
	End If
	If tmrToyBoxMBBonus.UserValue = 0 then 
		If (bToyBoxBonus3x) Then 			' Clear 3x or 2x Play Multipliers 
			AddPlayMultiplier 1/3
			I "Add for the 2nd lock"
			AddMultiball 2
			RealBallsInLock=RealBallsInLock-2 'todo check they are in there
			I "TOYBOX MB BONUS 3x: " & ballsOnPlayfield & " RealBall: " & RealBallsInLock
		Else
			AddPlayMultiplier 1/2	
			I "Add 1 for the 1 lock"
			AddMultiball 1
			RealBallsInLock=RealBallsInLock-1
			I "TOYBOX MB BONUS 2x: " & ballsOnPlayfield & " RealBall: " & RealBallsInLock
		End If 
		D "Done 2x/3x releasing ball(s)"
		bToyBoxBonus3x = False
		tmrToyBoxMBBonus.Enabled = False
		If bUsePUPDMD then puPlayer.LabelSet pBackglass,"MTimer",	""	,1,""

		for each a in aLockLights
			SetLightColor a, "green", 0
			a.uservalue=2  ' todo higher each time
		Next
	End If 
End Sub 

Sub AddPlayMultiplier(n)
	PlayMultiplier = PlayMultiplier * n
	If PlayMultiplier < 1 then PlayMultiplier = 1	' Dont go less than 0
End Sub

Dim dBall, dZpos, ballInScoop

Sub ScoopEject_hit  ' todo what If 2 balls come in?
	If ballInScoop then SoundSaucerKick 1, ScoopEject:ScoopEject.Kick 178 + (5 * Rnd()), 4:exit sub
	ballInScoop=True
	If BallsOnPlayfield = 1 then bPauseTimer=True
	Set dBall = ActiveBall
	dZpos = dBall.Z
	ScoopDelay=200

	ScoopEject.TimerInterval = 15
	ScoopEject.TimerEnabled = True
End Sub


Dim scoopdelay:scoopdelay=0

Sub ScoopEject_Timer
	Dim i
	dBall.Z = dZpos
	dZpos = dZpos-2
	If dZpos < -24 then me.timerinterval=1000 'slow down before you eject to buy some time
	If dZpos < -30 Then
		D "Exit Scoop......" & PlayerMode & " " & bMultiBallMode & " " & I34.state & " " & bBonusMode
		If Tilted Then
			ScoopEject.TimerEnabled = False
			vpmtimer.addtimer ScoopDelay, "ScoopKickOut '"
			Exit Sub
		End If
		ScoopEject.TimerEnabled = False
	
		If tmrSkillshot.Enabled and I35.state = 2 and bSkillShotsReady(1)=True Then ' Hold Left, Loop around and hit CIU, Left Orbit or Center Ramp
			D "SCORE SKILLSHOT"
			AddScore 1000000
			DOF 127, DOFPulse
			bSkillshotsReady(1) = False 

			QueueScene "ScenePlayMessage ""Video-0x0056.mp4"", ""SUPER"",""SKILL SHOT"","""" '", 2000, 1
			QueueScene "SceneClearPlayMessage '", 0, 1
			SmartButtonCount=SmartButtonCount+3
		End If 

		CheckModeProgress "I35"   ' if I34 is lit and ready for Medley or Tour ... set it and come back..
		MultiplierShot = 1		' Clear shot multiplier 

		if tmrMedleyTour.Enabled then 
		' Do nothing
			D "Scoop MedleyTour - skipping remainder of scoop checks"
		elseif tmrFinalTour.Enabled then
		' Do nothing
			D "Scoop Final Tour - skipping remainder of scoop checks"
		elseif bSecondMode then
		' Do nothing
			D "Scoop 2nd Mode"
		elseif I34.state <> 0 and PlayerMode = -1 and bMultiBallMode=False Then		' If we are in multiball dont go into player select
			If bBonusMode Then ' Earn the Super and proceed to the song choice..???"  TODO
				CheckModeProgress "I34"

				vpmtimer.addtimer ScoopDelay, "ScoopKickOut '"
				exit sub
			ElseIf bSecondMode Then
				StopSound Song
				PauseBigScore=True
				StopPlayerMode2
			End If
			puPlayer.LabelSet pBackglass,"BigScore"," ",0,""
			setModeSelectLight(False)
			EnablePlayerSelect

			If AutoQA or AutoAI Then
				PlayerMode=0
				i=INT(7*RND())
				PlayerMode = (PlayerMode + i)
				If (PlayerMode > 6) Then PlayerMode=PlayerMode-6
				i=7
				D "Select Song " & PlayerMode
				DO
					i=i-1
					PlayerMode = (PlayerMode + 1)
					If (PlayerMode > 6) Then PlayerMode=0
					If i=0 then exit do
				Loop While ModePercent(PlayerMode) >= 100
			Else
				SelectPlayerMode LeftFlipperKey		' Force them to select a mode
			End If
			UpdatePlayerMode
			If autoAI then vpmtimer.addtimer 1200,"Table1_KeyDown(RightMagnaSave) '"
			If autoAI then vpmtimer.addtimer 1600,"Table1_KeyUp(RightMagnaSave) '"
			Exit Sub	' Hold the ball until they select the next mode
		Else
			If I38.state <> 0 Then	' Mystery
				setMysteryLight(False)
				AwardMystery()
			End If
			If I33.state <> 0 Then
				setExtraBallLight(False)
				AwardExtraBall
			End If
		End If
		vpmtimer.addtimer ScoopDelay, "ScoopKickOut '" 
	End If
End Sub

Sub ScoopKickOut
	D "Scoop - eject ball"
	dBall.Z = 35
	ScoopEject.Kick 178 + (5 * Rnd()), 4
	ballInScoop=False
	bPauseTimer=False
	SoundSaucerKick 1, ScoopEject
End Sub

Dim eBall, eZpos, ballInLock
Sub ElevatorEject_hit
	If ballInLock then SoundSaucerKick 1, ElevatorEject:ElevatorEject.Kick 194, 14:Exit Sub	
	ballInLock=True
	If BallsOnPlayfield = 1 then bPauseTimer=True
	Set eBall = ActiveBall
	eZpos = eBall.Z
	ElevatorEject.TimerInterval = 15
	ElevatorEject.TimerEnabled = True
End Sub

Sub ElevatorEject_Timer
	Dim ScoopDelay
	eBall.Z = eZpos
	eZpos = eZpos-2
	If eZpos < -24 then me.timerinterval=1000 'slow down before you eject to buy some time
	If eZpos < -30 Then
		ElevatorEject.TimerEnabled = False
		ScoopDelay=200
		CheckModeProgress("I91")
		If I94.state <> 0 Then ' lock is lit
		FlashForMs I94, 200,50, 0
		ElevMultiBallCount=ElevMultiBallCount+1
		Select Case ElevMultiBallCount:
			Case 1:
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""BALL 1"",""LOCKED"","""" '", 2000, 2
				QueueScene "ScenePlayMessage ""Video-0x003D.mp4"", """","""","""" '", 3000, 2
				QueueScene "SceneClearPlayMessage '", 0, 1
				SetLightColor I110,"white", 2   
				SetLightColor I65, "white", 2
			Case 2:
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""BALL 2"",""LOCKED"","""" '", 2000, 2
				QueueScene "ScenePlayMessage ""Video-0x0039.mp4"", """","""","""" '", 3000, 2
				QueueScene "SceneClearPlayMessage '", 0, 1
				SetLightColor I110,"white", 2   
				SetLightColor I65, "white", 2
			Case 3:
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""BALL 3"",""LOCKED"","""" '", 2000, 2
				QueueScene "ScenePlayMessage ""Video-0x003B.mp4"", """","""","""" '", 3000, 2
				vpmtimer.addtimer 2000, "StartElevatorMB '"
				QueueScene "ScenePlayMessage ""Video-0x0034.mp4"", """","""","""" '", 8000, 2
				QueueScene "SceneClearPlayMessage '", 0, 1
				ScoopDelay=6500
		End Select

'show ball 1 Locked
'show animation
'If 3 balls then start MB 
' clear or reset the i94 lock
		End If
		vpmtimer.addtimer ScoopDelay, "ElevatorKickOut '" 
	End If
End Sub

Sub ElevatorKickOut
	D "Elevator - eject ball"
	eBall.Z = 35
	ElevatorEject.Kick 193, 45   ' 194 off the left wall
	ballInLock=False
	bPauseTimer=False
	FlashforMs F148, 200, 50, 0
	SoundSaucerKick 1, ElevatorEject
End Sub

Sub StartElevatorMB 
	dim a
	I "StartElevatorMB()"
	D "Turn Off ToyBoxLOCK!!!"
	SetLightColor I100, "green",0
	SetLightColor F147, "white",0 'toybox
	bElevMultiBall=True
	ElevMultiBallCount=0
	SetLightColor I23, "pink", 2		' Flash Elevator Multiball on lower playfield

	'If bUsePUPDMD then puPlayer.LabelSet pBackglass,"Timer",	"2000000"	,1,""
'
'	' store lights, light show, flash mode, start mode, end music, start music
'	' Mode is light all shots, get one, then light elevator, If elevator shot then score jackpot, and repeat for "each floor"
	PuPlayer.playlistplayex pMusic,"audioclear","clear.mp3",100, 1
	
	SaveMode=PlayerMode
	PlayerMode=8
	StartPlayerModeVideo False
	PlaySong "Song-" & PlayerMode & ".mp3"
	PlayerMode=SaveMode

	StackState(kStack_Pri1).Enable(8)
	For each a in aRampLights
		If a.name <> "I91" Then SSetLightColor kStack_Pri1, a, "pink", 2
	Next
	D "Lights and about to start mb"
	I "Adding 2 from the elevator"
	vpmtimer.addtimer 2000, "AddMultiball 2 '"
End Sub

sub target001_hit
	target001.isDropped=True
end sub

Sub sw1_Timer
	sw1.TimerEnabled = False
End Sub

Sub sw1_Hit()
	If Tilted then Exit Sub
	If sw1.TimerEnabled then Exit Sub
	sw1.TimerInterval = 1000
    sw1.TimerEnabled = True

    LaneBonus = LaneBonus + 1
    AddScore 100000
    LastSwitchHit = "sw1"
	If I11.state <> 0 Then
		FlashForMs I11, 200, 50, 0
		AwardSpecial()
	End If
	PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x0350laughing.wav",100, 1
	If bBallSaverActive then BallSaverActiveBuffer=BallSaverActiveBuffer+1
End Sub

Sub sw2_Timer
	sw2.TimerEnabled = False
End Sub

Sub sw2_hit		' left inlane
	If Tilted then Exit Sub
	If sw2.TimerEnabled then Exit Sub
	sw2.TimerInterval = 1000
    sw2.TimerEnabled = True
	If bWizardMode Then
		If PlayerMode3 = 4 Then
			AddScore(5000)
			CheckModeProgress("sw2")
		End If
		exit Sub
	End If

    LaneBonus = LaneBonus + 1
	If F152.state = 0 Then
		FlashForMs F152, 200, 50, 1
		AddScore 5000
		CheckDude()

		'CheckShotMultiplier()
	Else
		AddScore 10000
	End If
	CheckModeProgress("sw2")
	LastSwitchHit="sw2"
End Sub

Sub sw3_Timer
	sw3.TimerEnabled = False
End Sub

Sub sw3_hit		' left inner lane
	If Tilted then Exit Sub
	If sw3.TimerEnabled then Exit Sub
	sw3.TimerInterval = 1000
    sw3.TimerEnabled = True

	If bWizardMode Then
		If PlayerMode3 = 4 Then
			AddScore(5000)
			CheckModeProgress("sw3")
		End If
		exit Sub
	End If

    LaneBonus = LaneBonus + 1
	If F153.state = 0 Then
		FlashForMs F153, 200, 50, 1
		AddScore 5000
		CheckDude()

		'CheckShotMultiplier()
	Else
		AddScore 10000
	End If
	CheckModeProgress("sw3")
	LastSwitchHit="sw3"
End Sub

Sub sw5_Timer
	sw5.TimerEnabled = False
End Sub

Sub sw5_hit		' right inlane
	If Tilted then Exit Sub
	If sw5.TimerEnabled then Exit Sub
	sw5.TimerInterval = 1000
    sw5.TimerEnabled = True

	If bWizardMode Then
		If PlayerMode3 = 4 Then
			AddScore(5000)
			CheckModeProgress("sw5")
		End If
		exit Sub
	End If

    LaneBonus = LaneBonus + 1
	If F154.state = 0 Then
		FlashForMs F154, 200, 50, 1
		AddScore 5000
		CheckDude()

		'CheckShotMultiplier()
	Else
		AddScore 10000
	End If
	CheckModeProgress("sw5")
	LastSwitchHit="sw5"
End Sub

Sub sw6_Timer
	sw6.TimerEnabled = False
End Sub

Sub sw6_hit
	If Tilted then Exit Sub
	If sw6.TimerEnabled then Exit Sub
	sw6.TimerInterval = 1000
    sw6.TimerEnabled = True

	LaneBonus=LaneBonus+1
	AddScore 100000
	If I27.state <> 0 Then
		If bUsePUPDMD then 
			QueueScene "ScenePlayMessage ""Video-0x0066.mp4"", """","""&FormatScore(VipValue)&""","""" '", 2000, 2
			QueueScene "SceneClearPlayMessage '", 0, 2
		Else 
			DisplayDMDText2 "VIP PASS",FormatScore(VipValue), 1000, 11, 0
		End If
		AddScore VipValue
		FlashForMs I27, 200, 50, 0
		bLaneSaverEnabled=False
		If BallsOnPlayfield=1 Then
			bPauseTimer=True
		End If
		If bBallSaverActive = False Then
			BallSaverActiveBuffer=BallSaverActiveBuffer+1
		End If
	Else
		PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x0350laughing.wav",100, 1
	End If
	If bBallSaverActive then BallSaverActiveBuffer=BallSaverActiveBuffer+1
	LastSwitchHit="sw6"
End Sub

Sub CheckAerosmith()
	If SpellAerosmith=9 Then
		setMysteryLight(True)
		QueueScene "playmedia ""Video-0x0046.mp4"", ""PupMystery"", pOverVid , """", -1, """", 1, 1 '", 3000, 2
		' TODO lights the shot multiplier on the next shot you make for the rest of the ball
		bShotMultiplierSelect=True  ' Next shot gets the multiplier
		AddScore 75000
		playmedia "ARS107-Scene-100.mp4", "PupVideos", pPopUp5, "", -1, "", 1, 1
		'QueueScene "ScenePlayMessage ""ARS107-Scene-100.mp4"", """","""",""2X"" '", 2000, 2
		'QueueScene "SceneClearPlayMessage '", 0, 2
		vpmtimer.addtimer 200, "ResetAerosmith '" 
	Else
		If Int(5*Rnd()) < 2 and SpellAerosmith < 6 Then
			'QueueScene "ScenePlayMessage ""ARS107-Scene-99.mp4"", """","""","""" '", 2000, 2
			playmedia "ARS107-Scene-99.mp4", "PupVideos", pPopUp5, "", -1, "", 1, 1
'PuPlayer.playlistplayex 15,"PupVideos","ARS107-Scene-99.mp4",0,1
			'QueueScene "SceneClearPlayMessage '", 0, 2
		End If
	End If
	PuPlayer.LabelSet pBackglass, "Aerosmith", "PuPOverlays\\aerosmith-"&SpellAerosmith&".png",1, _
							"{'mt':2,'color':111111,'width':51, 'height':18,'yalign':0,'ypos':0.0,'xpos':50.4}"
End Sub

Sub ResetAerosmith()
	Dim a
	SpellAerosmith=0
	For each a in aerosmithLights
		a.State = 0
	Next
	PuPlayer.LabelSet pBackglass, "Aerosmith", "PuPOverlays\\aerosmith-"&SpellAerosmith&".png",1, _
							"{'mt':2,'color':111111,'width':51, 'height':18,'yalign':0,'ypos':0.0,'xpos':50.4}"
End Sub

Sub sw24_Timer
	sw24.TimerEnabled = False
End Sub

Sub sw24_hit  		' aerosmith A
	If Tilted then Exit Sub
	If sw24.TimerEnabled then Exit Sub
	sw24.TimerInterval = 1000
    sw24.TimerEnabled = True

	If I29.state=0 and not bSecondMode and not bMultiBallMode Then   ' debug do we still do this with SAME mode?
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I29, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I29")
	LastSwitchHit="sw24"
End Sub

Sub sw25_Timer
	sw25.TimerEnabled = False
End Sub

Sub sw25_hit  		' aerosmith E
	If Tilted then Exit Sub
	If sw25.TimerEnabled then Exit Sub
	sw25.TimerInterval = 1000
    sw25.TimerEnabled = True

	If I30.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I30, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I30")
	LastSwitchHit="sw25"
End Sub

Sub sw26_Timer
	sw26.TimerEnabled = False
End Sub

Sub sw26_hit  		' aerosmith R
	If Tilted then Exit Sub
	If sw26.TimerEnabled then Exit Sub
	sw26.TimerInterval = 1000
    sw26.TimerEnabled = True

	If I31.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I31, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I31")
	LastSwitchHit="sw26"
End Sub

Sub sw33_Timer
	sw33.TimerEnabled = False
End Sub

Sub sw33_hit  		' aerosmith H
	If Tilted then Exit Sub
	If sw33.TimerEnabled then Exit Sub
	sw33.TimerInterval = 1000
    sw33.TimerEnabled = True

	If I42.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I42, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I42")
	LastSwitchHit="sw33"
End Sub

Sub sw34_Timer
	sw34.TimerEnabled = False
End Sub

Sub sw34_hit  		' aerosmith T
	If Tilted then Exit Sub
	If sw34.TimerEnabled then Exit Sub
	sw34.TimerInterval = 1000
    sw34.TimerEnabled = True

	If I43.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I43, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I43")
	LastSwitchHit="sw34"
End Sub

Sub sw35_Timer
	sw35.TimerEnabled = False
End Sub

Sub sw35_hit  		' aerosmith I
	If Tilted then Exit Sub
	If sw35.TimerEnabled then Exit Sub
	sw35.TimerInterval = 1000
    sw35.TimerEnabled = True

	If I44.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I44, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I44")
	LastSwitchHit="sw35"
End Sub

Sub sw36_Timer
	sw36.TimerEnabled = False
End Sub

Sub sw36_hit  		' aerosmith M
	If Tilted then Exit Sub
	If sw36.TimerEnabled then Exit Sub
	sw36.TimerInterval = 1000
    sw36.TimerEnabled = True

	If I45.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I45, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I45")
	LastSwitchHit="sw36"
End Sub

Sub sw38_spin
	SoundSpinner()
	if NOT bWizardMode Then
		SpinHits=SpinHits - 1
		If SpinHits <=0 Then
			SpinValue=int(SpinValue*1.15)
			SpinHits=50
		End If
		SpinnerBonus=SpinnerBonus+1
	End If
	AddScore(SpinValue)
	SpinScoreCurrent=SpinScoreCurrent+SpinValue
	tmrSpin.Enabled=False
	tmrSpin.Enabled=True
	CheckModeProgress("sw38")
	' LastSwitchHit="sw38" caused problem as it continues to spinn
End Sub

Sub tmrSpin_Timer
	tmrSpin.Enabled=False
	SpinScore=SpinScoreCurrent
	SpinScoreCurrent=0
End Sub

Sub sw49_Timer
	sw49.TimerEnabled = False
End Sub

Sub sw49_hit  		' aerosmith O
	If Tilted then Exit Sub
	If sw49.TimerEnabled then Exit Sub
	sw49.TimerInterval = 1000
    sw49.TimerEnabled = True

	If I69.state=0 and not bSecondMode  and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			SetLightColor I69, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I69")
	LastSwitchHit="sw49"
End Sub

Sub sw50_Timer
	sw50.TimerEnabled = False
End Sub

Sub sw50_hit  		' aerosmith S		
	If Tilted then Exit Sub
	If sw50.TimerEnabled then Exit Sub
	sw50.TimerInterval = 1000
    sw50.TimerEnabled = True

	If I70.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			SetLightColor I70, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I70")
	LastSwitchHit="sw50"
End Sub

Sub sw52_hit  		' left orbit
    If Tilted Then Exit Sub
	If sw52.TimerEnabled then Exit Sub
	sw52.TimerInterval = 1000
    sw52.TimerEnabled = True

	D "sw52_hit Last=" & LastSwitchHit

	If LastSwitchHit = "sw52pre" Then 	' We are going up the ramp
		D "I62.state=" & I62.state
		If tmrSkillshot.Enabled and I62.state = 2 and bSkillshotsReady(1)=True Then		' Hold Left, Loop around and hit CIU, Left Orbit or Center Ramp
			D "SCORE SKILLSHOT"
			AddScore 1000000
			DOF 127, DOFPulse
			bSkillshotsReady(1) = False 

			QueueScene "ScenePlayMessage ""Video-0x0056.mp4"", ""SUPER"",""SKILL SHOT"","""" '", 2000, 1
			QueueScene "SceneClearPlayMessage '", 0, 1
			SmartButtonCount=SmartButtonCount+3
		End If 
	Else
		If tmrSkillshot.Enabled then 
			tmrSkillshot_Timer()
		End If
	End If 
	If LastSwitchHit="swlooper" Then ' full orbit
		ClearShotMultiplier(I107)
		CheckModeProgress "I107"
		AddScore 3000
		MultiplierShot = 1

		If I110.state <> 0 Then
			SetLightColor I110, "white", 0
			Addscore 10000
			LoopBonus=LoopBonus+1
			If I65.state <> 0 Then ' need the 2nd elevator
					' If elevator light is on then 1/2 of elevator - If both are out then award elevator					
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""COMPLETE ORBITS"",""TO LIGHT"",""ELEVATOR LOCK"" '", 2000, 2
				QueueScene "SceneClearPlayMessage '", 0, 2
			Else
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""ELEVATOR"",""LIT"","""" '", 2000, 2
				QueueScene "SceneClearPlayMessage '", 0, 2
				'99999
				SetLightColor  I94, "white", 2
			End If
		End If
	End If

	LastSwitchHit="sw52"
End Sub

Sub sw52_Timer
	sw52.TimerEnabled = False
End Sub

Sub sw52pre_hit
	LastSwitchHit="sw52pre"
End Sub


Sub sw53_hit  		' center ramp
    If Tilted Then Exit Sub
	D "sw53_hit"

	If tmrSkillshot.Enabled and I86.state = 2 and bSkillShotsReady(1)=True Then		
									' Hold Left, Loop around and hit CIU, Left Orbit or Center Ramp
		D "SCORE SKILLSHOT"
		AddScore 1000000
		DOF 127, DOFPulse
		bSkillshotsReady(1) = False 
		QueueScene "ScenePlayMessage ""Video-0x0056.mp4"", ""SUPER"",""SKILL SHOT"","""" '", 2000, 2
		QueueScene "SceneClearPlayMessage '", 0, 2
		SmartButtonCount=SmartButtonCount+3
	End If 

	ClearShotMultiplier(I86)
	CheckModeProgress "I86"
	AddScore 5000

	RampBonus=RampBonus+1

	MultiplierShot = 1		' Clear shot multiplier 

	If AutoAI and BallsOnPlayfield = 1 Then
		If F152.state<>0 Then ' try for a dude light 
			Table1_KeyDown(LeftFlipperKey)
			vpmtimer.addtimer 200,"Table1_KeyUp(LeftFlipperKey) '"
		End If
	End If
	LastSwitchHit="sw53"
End Sub

Sub sw56_Hit
	If Tilted Then Exit Sub
	If sw56.TimerEnabled then Exit Sub
	sw56.TimerInterval = 2000
    sw56.TimerEnabled = True
	D "sw56 Toy Box"
  ' TODO perhaps base this on F147
	If bElevMultiBall then Exit Sub    ' no toy box during elevator mode
	if tmrMedleyTour.Enabled Then
		CheckModeProgress("I81")
		Exit Sub
	End If
	if tmrFinalTour.Enabled Then
		CheckModeProgress("I81")
		Exit Sub
	End If

	If I75.state=0 Then   ' Light the Locks
		I75.uservalue=I75.uservalue-1
		If I75.uservalue<=0 then
			SetLightColor I100, "green", 2
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""LOCK"",""IS LIT"","""" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
			I75.blinkinterval=125:I75.state=2
		Else
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 MORE"",""HIT"",""TO LIGHT LOCK"" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
		End If
	ElseIf I76.state=0 Then
		I76.uservalue=I76.uservalue-1
		If I76.uservalue<=0 then
'TODO		If bToyBoxMultiball and BallsOnPlayfield > 1 then
			SetLightColor I100, "green", 2
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""LOCK"",""IS LIT"","""" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
			I76.blinkinterval=125:I76.state=2
		Else
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 MORE"",""HIT"",""TO LIGHT LOCK"" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
		End if
	ElseIf I77.state=0 and not bToyBoxMultiball Then
		I77.uservalue=I77.uservalue-1
		If I77.uservalue<=0 then
			SetLightColor I100, "green", 2
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""LOCK"",""IS LIT"","""" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
			I77.blinkinterval=125:I77.state=2
		Else
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 MORE"",""HIT"",""TO LIGHT LOCK"" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
		End If
	ElseIf I78.state=0 and not bToyBoxMultiball Then
		I78.uservalue=I78.uservalue-1
		If I78.uservalue<=0 then
			SetLightColor I100, "green", 2
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""LOCK"",""IS LIT"","""" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
			I78.blinkinterval=125:I78.state=2
		Else
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 MORE"",""HIT"",""TO LIGHT LOCK"" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
		End If
	ElseIf I79.state=0 and not bToyBoxMultiball Then
		I79.uservalue=I79.uservalue-1
		If I79.uservalue<=0 then
			SetLightColor I100, "green", 2
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""LOCK"",""IS LIT"","""" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
			I79.blinkinterval=125:I79.state=2
		Else
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 MORE"",""HIT"",""TO LIGHT LOCK"" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
		End If
	ElseIf I80.state=0 and not bToyBoxMultiball Then
		I80.uservalue=I80.uservalue-1
		If I80.uservalue<=0 then
			SetLightColor I100, "green", 2
			SetLightColor F147, "white", 0
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""LOCK"",""IS LIT"","""" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
			I80.blinkinterval=125:I80.state=2
			SetLightColor F147, "white", 0
		Else
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 MORE"",""HIT"",""TO LIGHT LOCK"" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
		End If
	End If
	if 10*Rnd() <2 then playmedia "","audio-jacky",pCallouts,"",1500,"",1,2
	CheckModeProgress("I81")
	CheckShotMultiplier()
End Sub

Sub sw56_Timer
	sw56.TimerEnabled = False
End Sub

Sub sw57_hit  		' toybox Left
	If Tilted Then Exit Sub
	If sw57.TimerEnabled then Exit Sub
	D "sw57"
	sw57.TimerInterval = 1000
    sw57.TimerEnabled = True
	If I72.state <> 0 Then  ' Todo .. perhaps make it harder , first hit goes to blink
		AddScore 1000
	Else ' light it up, If both are lit then show VIP Pass
		AddScore 2000
		If I73.state <> 0 Then 		'both are lit
			If I27.state=0 Then 	' no VIP yet
				SetLightColor I27, "white", 2
				If bUsePUPDMD then 
					QueueScene "ScenePlayMessage ""Video-0x0066.mp4"", """",""LIT"","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "VIP PASS","LIT", 1000, 11, 0
				End If
 
			Else ' Increase value of VIP since it is lit already
				AddScore 5000
				VipValue=VipValue+50000
				' TODO show VIP Value
				If bUsePUPDMD then 
					QueueScene "ScenePlayMessage ""Video-0x0066.mp4"", """","""&"AT  " & FormatScore(VipValue) & ""","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "VIP VALUE","AT "&FormatScore(VipValue), 1000, 11, 0
				End If
			End If
			SetLightColor I72, "white", 0:SetLightColor I73, "white", 0
		Else
			SetLightColor I72, "white", 1
			' TODO Show stars
			If I27.state = 0 Then  ' already have VIP Pass
				If bUsePUPDMD then 
					QueueScene "ScenePlayMessage ""Video-0x0067.mp4"", ""1 MORE FOR"",""VIP PASS"","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "1 MORE FOR","VIP PASS", 1000, 11, 0
				End If
			Else
				If bUsePUPDMD then 
					QueueScene "ScenePlayMessage ""Video-0x0067.mp4"", ""1 MORE FOR"",""VIP PASS"","""&"VIP PASS ="&FormatScore(VipValue)&""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "1 MORE FOR","VIP PASS", 1000, 11, 0
				End If
			End If
		End If
	End If
	LastSwitchHit="sw57"
End Sub

Sub sw57_Timer
	sw57.TimerEnabled = False
End Sub

Sub sw58_hit 		' toybox right
	If Tilted Then Exit Sub
	If sw58.TimerEnabled then Exit Sub
	sw58.TimerInterval = 1000
    sw58.TimerEnabled = True
	If I73.state <> 0 Then
		AddScore 1000
	Else ' light it up, If both are lit then show VIP Pass
		AddScore 2000
		If I72.state <> 0 Then 		'both are lit
			If I27.state=0 Then 	' no VIP yet
				SetLightColor I27, "white", 2
				If bUsePUPDMD then 
					QueueScene "ScenePlayMessage ""Video-0x0066.mp4"", """",""LIT"","""" '", 3000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "VIP PASS","LIT", 1000, 11, 0
				End If
			Else ' Increase value of VIP since it is lit already
				AddScore 5000
				VipValue=VipValue+50000
				' TODO show VIP Value
				If bUsePUPDMD then 
					QueueScene "ScenePlayMessage ""Video-0x0066.mp4"", """","""&"AT "&FormatScore(VipValue)&""","""" '", 3000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "VIP VALUE AT","AT "&FormatScore(VipValue), 1000, 11, 0
				End If
			End If
			SetLightColor I72, "white", 0:SetLightColor I73, "white", 0
		Else
			SetLightColor I73, "white", 1
			' TODO Show stars
			If I27.state = 0 Then  ' already have VIP Pass
				If bUsePUPDMD then 
					QueueScene "ScenePlayMessage ""Video-0x0067.mp4"", ""1 MORE FOR"",""VIP PASS"","""" '", 3000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "1 MORE FOR","VIP PASS ", 1000, 11, 0
				End If
			Else
				If bUsePUPDMD then 
					QueueScene "ScenePlayMessage ""Video-0x0067.mp4"", ""1 MORE FOR"",""VIP PASS"","""&"VIP PASS = "&FormatScore(VipValue)&""" '", 3000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "1 MORE FOR","VIP PASS", 1000, 11, 0
				End If
			End If
		End If
	End If
	LastSwitchHit="sw58"
End Sub

Sub sw58_Timer
	sw58.TimerEnabled = False
End Sub


Sub sw62_hit		' right orbit
    If Tilted or bSkillshotsReady(1) or LastSwitchHit = "ballsavestarttrigger" Then Exit Sub
	If sw62.TimerEnabled then Exit Sub
	sw62.TimerInterval = 1000
    sw62.TimerEnabled = True

	If LastSwitchHit="swlooper" Then ' full orbit
		ClearShotMultiplier(I62)
		CheckModeProgress "I62"
		AddScore 3000
		MultiplierShot = 1

		If I65.state <> 0 Then
			SetLightColor I65, "white", 0
			Addscore 10000
			LoopBonus=LoopBonus+1
			If I110.state <> 0 Then ' need the 2nd elevator
					' If elevator light is on then 1/2 of elevator - If both are out then award elevator					
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""COMPLETE ORBITS"",""TO LIGHT"",""ELEVATOR LOCK"" '", 2000, 2
				QueueScene "SceneClearPlayMessage '", 0, 2
			Else
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""ELEVATOR"",""LIT"","""" '", 2000, 2
				QueueScene "SceneClearPlayMessage '", 0, 2
				'99999
				SetLightColor I94, "white", 2   
			End If
		End If
	End If

	LastSwitchHit="sw62"
End Sub

Sub sw62_Timer
	sw62.TimerEnabled = False
End Sub

Sub ballsavestarttrigger_hit
	LastSwitchHit = "ballsavestarttrigger"
End Sub

Sub sw62pre_hit
	LastSwitchHit="sw62pre"
End Sub

Sub sw63_hit ' Right Ramp001
	If Tilted Then Exit Sub
	D "sw63"
	If sw63.TimerEnabled then Exit Sub
	sw63.TimerInterval = 1000
    sw63.TimerEnabled = True

	ClearShotMultiplier(I103)
	CheckModeProgress "I103"
	AddScore 5000

	RampBonus=RampBonus+1

	MultiplierShot = 1		' Clear shot multiplier 

	If AutoAI and BallsOnPlayfield = 1 Then
		If F154.state<>0 Then ' try for a dude light 
			Table1_KeyDown(RightFlipperKey)
			vpmtimer.addtimer 200,"Table1_KeyUp(RightFlipperKey) '"
		End If
	End If
	LastSwitchHit="sw63"
End Sub

Sub sw63_Timer
	sw63.TimerEnabled = False
End Sub

Sub swlooper_hit
	D "swlooper: LastSwHit=" & LastSwitchHit
	If LastSwitchHit <> "ballsavestarttrigger" Then 
		LastSwitchHit="swlooper" ' dont credit the loop on a plunge
	End if
End Sub

Sub sw_shortplunge_hit
	If tmrSkillshot.Enabled and bSkillShotsReady(1)=False Then
		tmrSkillshot_Timer()
	End If
End Sub

Sub CheckDude()
	If F152.state<>0 AND F153.state<>0 AND F154.state<>0 Then
		FlashForMs F152, 100, 20, 0
		FlashForMs F153, 100, 20, 0
		FlashForMs F154, 100, 20, 0
		AddBonusMultiplier 1
	End If
End Sub

Dim ShotMultiplierStrobe
Sub ClearShotMultiplier(light)
	D "ClearShotMultiplier " & light.name
	If light.State = 2 then ' Blinking Multiplier 
		If (tmrShotMultiplierStrobe.Enabled = False) Then 	' Hit a shot on the Multiplier
			light.UserValue = 1		' Mark we hit this one
			SetLightColor I32, "green", I32.UserValue
			SetLightColor I67, "green", I67.UserValue
			SetLightColor I84, "green", I84.UserValue
			SetLightColor I89, "green", I89.UserValue
			SetLightColor I95, "green", I95.UserValue
			SetLightColor I101,"green", I101.UserValue
			SetLightColor I46, "green", I46.UserValue
			SetLightColor I112, "green", I112.UserValue
						
'			PlaySoundVol "2xMultiplier", VolDef
			bShotMultiplierSelect = False
'			vpmtimer.addtimer 1500, "PlaySoundVol ""RovingShotMultiplierIsLit"", VolDef '"
'			ShotMultiplierStrobe=0
			tmrShotMultiplierStrobe.Enabled = True
'			SetLightColor lDrax_m, "green", 2
		Else												' We are in Strobing mode 
			tmrShotMultiplierStrobe.Enabled = False
			SetLightColor I32, "green", I32.UserValue
			SetLightColor I67, "green", I67.UserValue
			SetLightColor I84, "green", I84.UserValue
			SetLightColor I89, "green", I89.UserValue
			SetLightColor I95, "green", I95.UserValue
			SetLightColor I101,"green", I101.UserValue
			SetLightColor I46, "green", I46.UserValue
			SetLightColor I112,"green", I112.UserValue
			Multiplier3x = 3						' 3x is just for this shot
		End If
	ElseIf light.state = 1 then ' Solid lit setup 2x for the shot
		MultiplierShot = 2
	End If 
End Sub


Sub tmrShotMultiplierStrobe_Timer
	Select Case ShotMultiplierStrobe
		Case 0:
			SetLightColor I32, "green", I32.UserValue
			SetLightColor I67, "green", 2
		Case 1:
			SetLightColor I67, "green", I67.UserValue
			SetLightColor I84, "green", 2
		Case 2:
			SetLightColor I84, "green", I84.UserValue
			SetLightColor I89, "green", 2
		Case 3:
			SetLightColor I89, "green", I89.UserValue
			SetLightColor I95, "green", 2
		Case 4:
			SetLightColor I95, "green", I95.UserValue
			SetLightColor I101, "green", 2
		Case 5:
			SetLightColor I101, "green", I101.UserValue
			SetLightColor I46, "green", 2
		Case 6:
			SetLightColor I46, "green", I46.UserValue
			SetLightColor I112, "green", 2
		Case 7:
			SetLightColor I112, "green", I112.UserValue
			SetLightColor I32, "green", 2
	End Select
	ShotMultiplierStrobe=ShotMultiplierStrobe+1
	If ShotMultiplierStrobe>7 then ShotMultiplierStrobe=0
End Sub

Sub CheckShotMultiplier
	D "CheckShotMultiplier -- TODO"
End Sub

Function addsuffix(num)
	Dim suff
    Select Case num
         Case 1 : suff = "ST"
         Case 2 : suff = "ND"
         Case 3 : suff = "RD"
         Case 4, 5, 6, 7, 8, 9: suff = "TH"
    End Select
    AddSuffix = cStr(num) + suff
End Function

sub DumpValues
dim a,b
b=0
for each a in aRampLights
	D "ToyboxMB " & a.name & " " & Shots(b)
	b=b+1
Next
end sub

Sub CheckModeProgress(trigger_name)  ' rats is done after any attmept, same with back and walk -- get super and award
	dim bValidHit
	dim bFinalShot
	dim hitLight
	dim a, b, c, i 
	dim thisScore, vidToShow
	dim MB_Multiplier:MB_Multiplier=1
	Dim bModeComplete
	Dim bDoubleBonus, bGotLight
	bDoubleBonus = False
	bModeComplete = False
	bValidHit = False

	D "CheckModeProgress " & trigger_name & " PlayerMode=" & PlayerMode & " 2:" & PlayerMode2 & " bSecondMode=" & bSecondMode & " bonusMode:" & bBonusMode

	If bDebounce Then		' we cant hit it twice or in between resets
		exit Sub
	End If

	If BallSearchCnt > 0 then exit sub  ' We are in an error mode skip doing anything
	ResetBallSearch

' Check For Any OTHER Remaining Super Awards  ie not the current PlayerMode2 one
	If not bWizardMode Then
		If (trigger_name = "I62" or trigger_name = "I107") and PlayerMode2 <> 0 Then
			If Mode2Percent(0) <> -1 and Mode2Percent(0) < 100 Then
				PlayProgress trigger_name

				Mode2Progress(0) = Mode2Progress(0)+1
				Mode2Percent(0) = CINT((Mode2Progress(0)  / 10) * 100)
				AddScore Mode2Value(0)			' 

				ShowPlayerMode2(0)
				If (Mode2Progress(0) >= 10) Then
					Mode2Percent(0) = 100
					QueueScene "SceneClearPlayMessage '", 0, 2
				End If
			End If
		End If

		If (trigger_name = "I86" or trigger_name = "I103") and PlayerMode2 <> 1 Then
			If Mode2Percent(1) <> -1 and Mode2Percent(1) < 100 Then
				PlayProgress trigger_name

				Mode2Progress(1) = Mode2Progress(1)+1
				Mode2Percent(1) = CINT((Mode2Progress(1)  / 10) * 100)
				AddScore Mode2Value(1)		' 

				ShowPlayerMode2(1)
				If (Mode2Progress(1) >= 10) Then
					Mode2Percent(1) = 100
					QueueScene "SceneClearPlayMessage '", 0, 2
				End If
			End If
		End If
	
		If Mode2Percent(2) <> -1 and Mode2Percent(2) < 100 and PlayerMode2 <> 2 Then
			For each a in aerosmithLights
				If a.name = trigger_name Then
					PlayProgress trigger_name

					Mode2Progress(2) = Mode2Progress(2)+1
					Mode2Percent(2) = CINT((Mode2Progress(2)  / 20) * 100)
					AddScore Mode2Value(2)		' 

					ShowPlayerMode2(2)
					If (Mode2Progress(2) >= 20) Then
						Mode2Percent(2) = 100
						QueueScene "SceneClearPlayMessage '", 0, 2
					End If
					exit for
				End If
			Next
		End If
' scoring  - handled in the Scoring Routine

		If (trigger_name = "sw2" or trigger_name = "sw3" or trigger_name = "sw5") and PlayerMode2 <> 4 Then
			If Mode2Percent(4) <> -1 and Mode2Percent(4) < 100 Then
				PlayProgress trigger_name

				Mode2Progress(4) = Mode2Progress(4)+1
				Mode2Percent(4) = CINT((Mode2Progress(4)  / 10) * 100)
				AddScore Mode2Value(4)	' 

				ShowPlayerMode2(4)
				If (Mode2Progress(4) >= 10) Then
					Mode2Percent(4) = 100
					QueueScene "SceneClearPlayMessage '", 0, 2
				End If
			End If
		End If

		If trigger_name = "bumper" Then
			If Mode2Percent(5) <> -1 and Mode2Percent(5) < 100 and PlayerMode2 <> 5 Then
				PlayProgress trigger_name

				Mode2Progress(5) = Mode2Progress(5)+1
				Mode2Percent(5) = CINT((Mode2Progress(5)  / 50) * 100)
				AddScore Mode2Value(5)

				ShowPlayerMode2(5) '7todo
				If (Mode2Progress(5) >= 50) Then
					Mode2Percent(5) = 100
					QueueScene "SceneClearPlayMessage '", 0, 2
				End If
			End If
		End If

		If trigger_name = "sw38" Then
			If Mode2Percent(6) <> -1 and Mode2Percent(6) < 100 and PlayerMode2 <> 6 Then
				PlayProgress trigger_name

				Mode2Progress(6) = Mode2Progress(6)+1
				Mode2Percent(6) = CINT((Mode2Progress(6)  / 100) * 100)
				AddScore Mode2Value(6)			' 

				ShowPlayerMode2(6)
				If (Mode2Progress(6) >= 100) Then
					Mode2Percent(6) = 100
					QueueScene "SceneClearPlayMessage '", 0, 2
				End If
			End If
		End If
	End If


'TODO  2ssssss
	If bToyBoxMultiball then ' check MB Lights
		c=0
		for each a in aRampLights
			If a.name = trigger_name and StackState(kStack_Pri1).GetArrowState(a) <> 0 Then
				Shots(c)=Shots(c)-1
				ToyBoxMBJackpotHits=ToyBoxMBJackpotHits+1
				DumpValues
				D "ToyBoxMB: JackpotHits: " & ToyBoxMBJackpotHits & " " & a.name & " c=" & c
				If shots(c) <= 0 or ToyBoxMBJackpotHits >= 16 then 
					SSetLightColor kStack_Pri1, a, "white", 0 ' ScoreJackPot
				end if
				If ToyBoxMBJackpotHits <=7 then
					ToyBoxMBJackpot=ToyBoxMBJackpotBase
					QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", """","""&FormatScore(ToyBoxMBJackpot)&""","""" '", 1000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
					AddScore ToyBoxMBJackpot
				ElseIf ToyBoxMBJackpotHits=8 Then
					ToyBoxMBJackpot=4000000
					QueueScene "ScenePlayMessage ""Video-0x0091.mp4"", ""SUPER JACKPOT"","""&FormatScore(ToyBoxMBJackpot)&""","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
					AddScore ToyBoxMBJackpot
				ElseIf ToyBoxMBJackpotHits >8 and ToyBoxMBJackpotHits < 16 Then
					ToyBoxMBJackpot=ToyBoxMBJackpotBase
					QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", """","""&FormatScore(ToyBoxMBJackpot)&""","""" '", 1000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
					AddScore ToyBoxMBJackpot
				ElseIf ToyBoxMBJackpotHits=16 then
					ToyBoxMBJackpot=4000000
					QueueScene "ScenePlayMessage ""Video-0x0090.mp4"", ""SUPER JACKPOT"","""&FormatScore(ToyBoxMBJackpot)&""","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
					AddScore ToyBoxMBJackpot
' Turn off all Shots and light one random shot
					SSetLightColor kStack_Pri1, aRampLights(bRndModeShot), "green", 2
					AddScore ToyBoxMBJackpot
				ElseIf ToyBoxMBJackpotHits=17 then
					ToyBoxMBJackpot=3500000
					QueueScene "ScenePlayMessage ""Video-0x008F.mp4"", ""SUPER JACKPOT"","""&FormatScore(ToyBoxMBJackpot)&""","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
' Light one last DSJ at Lock shot
					AddScore ToyBoxMBJackpot
					SSetLightColor kStack_Pri1, I97, "green", 2
				ElseIf ToyBoxMBJackpotHits=18 then
					ToyBoxMBJackpot=8000000
					QueueScene "ScenePlayMessage ""Video-0x0065.mp4"", ""DOUBLE"","""&FormatScore(ToyBoxMBJackpot)&""",""SUPER JACKPOT"" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
' reset - show all locks and start over
' make the Insert Solid
					SetLightColor I22, "green", 1
					i=0
					for each b in aRampLights
						SSetLightColor kStack_Pri1, b, "green", 2
						shots(i)=2
						i=i+1
					Next
					ToyBoxMBJackpotHits=0
					AddScore ToyBoxMBJackpot
				End If
				ToyBoxMBJackPotTotal=ToyBoxMBJackpotTotal+ToyBoxMBJackpot
				Exit For
			End If
			c=c+1
		Next
	elseIf bElevMultiBall Then ' If all lights then turn them off and award then light elevator again
		If trigger_name = "I91" and StackState(kStack_Pri1).GetArrowState(I91) <> 0 Then 'score Floor Jackpot and turn remaining lights on
		D "##Lovel Elevator - Hit elevator lock"
			SSetLightColor kStack_Pri1, I91, "pink", 0
			ElevMBJackpotHits=ElevMBJackpotHits+1
			If ElevMBJackpotHits=6 Then SetLightColor I23, "pink", 1
' Elev tunnel and 2nd FLOOR JACKPOT 1M  each floor
			ElevMBJackpot=1000000*ElevMBJackpotHits
			ElevMBJackpotTotal=ElevMBJackpotTotal+ElevMBJackpot
			QueueScene "ScenePlayMessage ""Video-0x003E.mp4"", """&addSuffix(ElevMBJackpotHits)&" FLOOR"",""JACKPOT"","""&FormatScore(ElevMBJackpot)&""" '", 3000, 1
			QueueScene "ScenePlayMessage ""Video-0x0036.mp4"", """","""","""" '", 3000, 1
			QueueScene "SceneClearPlayMessage '", 0, 1
			AddScore ElevMBJackpot 
			For each a in aRampLights
				If a.name <> "I91" then 
					If StackState(kStack_Pri1).GetArrowColor(a) = "pink" Then ' check color here
						SSetLightColor kStack_Pri1, a, "pink", 2 ' Only relight the ones we need here ... 
					End If
				end if
			Next
		Else
			For each a in aRampLights
				If a.name = trigger_name and StackState(kStack_Pri1).GetArrowState(a) <> 0 then ' turn off lights and light Elev for jackpot
					D "##Love Elevator - hit shot " & a.name
					SSetLightColor kStack_Pri1, a, "pink", 0 ' ScoreJackPot
' Spinning mp4 and "JACKPOT 500000 + 25K for each floor - dont relight that mode
					QueueScene "ScenePlayMessage ""Video-0x0037.mp4"", ""JACKPOT"","""&FormatScore(int(500000+(25000*ElevMBJackpotHits)))&""","""" '", 3000, 1
					QueueScene "SceneClearPlayMessage '", 0, 1
					bValidHit=True
					AddScore int(500000+(25000*ElevMBJackpotHits))
					Exit For
				end if
			Next
			If bValidHit Then
				D "##Love Elevator - turn off all pink lights to 0"
				For each b in aRampLights
					If StackState(kStack_Pri1).GetArrowColor(b) = "pink" Then   ' Turn the remaining pink lights off
						SSetLightColor kStack_Pri1, b, "pink", 0
					End If
				Next
				D "##Love Elevator - turn elevator back on"
				SSetLightColor kStack_Pri1, I91, "pink", 2   ' elevator lock
			End If
		End If
	end If


	If CheckWizardModeStart(trigger_name) then Exit Sub				' Wizard Modes Stack so we check here, bail out when we Start 
	CheckWizardModeProgress(trigger_name)

	If bBonusMode Then 
		D "checkmodeprogress: Checking BONUSMODE"
		bValidHit=False
		for each a in aRampLights
			If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then
				D "Checking " & a.name &" and status: " & StackState(kStack_Pri0).GetArrowState(a)
				D ">>>>>>>START SUPER MODE :" & PlayerMode & " 2:" & PlayerMode2
				bValidHit=True
				exit for
			end if
		next
		If bValidHit Then
			for each a in aRampLights
				SSetLightColor kStack_Pri0, a, modeRampColor, 0
			next
			bBonusMode=False
			bSecondMode=True
			StartPlayerMode2
			exit sub
		end if
	End If

	D "Check progress 2nd: " & bSecondMode & " " & PlayerMode & " " & trigger_name
	If bWizardMode Then
		' Do Nothing
	ElseIf (bSecondMode) Then
		If trigger_name = "I35" and bMultiBallMode=False Then		' scoop hit
			StopPlayerMode2
		Else
			CheckModeProgress2nd(trigger_name)
		End If 
	Else
		If PlayerMode <> -1 Then
			D "Mode Progress Percent: " & ModePercent(PlayerMode) & " PlayerMode: " & PlayerMode
			If ModePercent(PlayerMode) >= 100 then 
				D "Skip Score: " & ModePercent(PlayerMode)
				Exit Sub 
			End If 
		End If 

		' Randomly Award Jester for Non Lit Shot
			If int(Rnd()*100) < 15 Then
				For each a in aRampLights
					If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) = 0 then
						HiddenJesters=HiddenJesters-1
						If HiddenJesters <= 0 Then
							If I33.state = 0 Then
								setExtraBallLight(True)
								HiddenJesters=25
							Else ' dont try and award jester yet
								HiddenJesters=HiddenJesters+1
							End If
						End If
						Exit For
					End If
				Next
			End If


		D "Checking each of the playermodes :" & PlayerMode
		Select Case PlayerMode
		Case -1: 'No Mode Selected
        Case 0: '   last child    CIU lights F151   TODO
				' left &right flash, center, right solid, make both ramps to relight them keep making orbits - 8 shots
			D "Looping thru the ramps trigger_name=" & trigger_name 
			For each a in aRampLights
				If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
					bDebounce = True	
					D "Match trigger_name=" & trigger_name & " Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)
					' So the ball doesnt roll over twice before we reset
					vpmtimer.addtimer 400, "debounceReset '"
						'PlaySound "gg_RampHit"					' Play sounds and reset the timer to reset the lights
					'LightSeqScore.Play SeqUpOn, 100, 0		 
					PlayProgress trigger_name
					
					If StackState(kStack_Pri0).GetArrowState(a) = 1 Then  ' If solid then turn off
						AddScore 15000
						FlashForMs a, 200, 50, 0
						SSetLightColor kStack_Pri0, a, modeRampColor, 0 
					End If 
					If StackState(kStack_Pri0).GetArrowState(I86)=0 and StackState(kStack_Pri0).GetArrowState(I103)=0 and not bModeProgressUpgraded then
						' relight them
						D "SSetLightColorName kStack_Pri0, ""I86"", """ & modeRampColor & """, 1 '"  ' 
						vpmtimer.addtimer 200, "SSetLightColorName kStack_Pri0, ""I86"", """ & modeRampColor & """, 1 '"  ' delay 
						vpmtimer.addtimer 200, "SSetLightColorName kStack_Pri0, ""I103"",""" & modeRampColor & """, 1 '"  ' delay
					End if
					If StackState(kStack_Pri0).GetArrowState(a) = 2 Then  ' If blink then quick blink and return to blink
						AddScore 25000
						If bModeProgressUpgraded Then
							FlashForMs a, 200, 50, 0
						Else
							vpmtimer.addtimer 100,  a.name & ".blinkinterval=20 '"
							vpmtimer.addtimer 1000, a.name & ".blinkinterval=" & BlinkIntDef & " '"
						End If
					End If
					
					ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
					Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
					If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then 
						setCIULight(True)
					End If
					ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 8) * 100)  ' 8 shots per this mode
					RefreshPlayerMode 
					If (ModeProgress(PlayerMode) = 8) Then   ' 8 shots
						ModePercent(PlayerMode) = 100
						bModeComplete = True
						pDMDEvent(541)  ' Loops
						setCIULight(False)
					End If
				End if
			Next
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade
				setCIULight(False)
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1

				bModeProgressUpgraded=True
				If bHardMode Then ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 2000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
				bModeProgressUpgraded=True
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 2000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
				For each a in aRampLights				' Upgrade to solid
					If StackState(kStack_Pri0).GetArrowState(a) = 0 then
						SSetLightColor kStack_Pri0, a, modeRampColor, 1
					End If
				Next
			End If 
		Case 1:  ' walk   need both orbits for them to relight, after the center ramp the right ramp is lit, after right the center
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade
				setCIULight(False)
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1
				bModeProgressUpgraded=True
				If bHardMode Then ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 2000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
				For each a in aRampLights				' Upgrade so All the arrows count now
					SSetLightColor kStack_Pri0, a, modeRampColor, 1
				Next
			else 
				D "Looping thru the ramps trigger_name=" & trigger_name 
				For each a in aRampLights
					If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
						D "Match trigger_name=" & trigger_name & "Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)
						bDebounce = True							' So the ball doesnt roll over twice before we reset
						vpmtimer.addtimer 400, "debounceReset '"

						'PlaySound "gg_RampHit"					' Play sounds and reset the timer to reset the lights
						'LightSeqScore.Play SeqUpOn, 100, 0		 
						PlayProgress trigger_name
						AddScore 25000
						SSetLightColor kStack_Pri0, a, modeRampColor, 0
						If not bModeProgressUpgraded Then 'cycle the lights
							If a.name = "I86" Then
								SSetLightColor kStack_Pri0, I97, modeRampColor, 2
							elseIf a.name = "I97" Then
								SSetLightColor kStack_Pri0, I86, modeRampColor, 2
							elseIf a.name = "I107" Then
								If StackState(kStack_Pri0).GetArrowState(I62)=0 Then  ' both are are off so they relight
									SSetLightColor kStack_Pri0, I62, modeRampColor, 2
									SSetLightColor kStack_Pri0, I107,modeRampColor, 2
								End If
							elseIf a.name = "I62" Then
								If StackState(kStack_Pri0).GetArrowState(I107)=0 Then ' both are are off so they relight
									SSetLightColor kStack_Pri0, I62, modeRampColor, 2
									SSetLightColor kStack_Pri0, I107,modeRampColor, 2
								End If
							End If
						End If
						
						ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
						Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
						If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then
							setCIULight(True)
						End If
						ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 10) * 100)
						RefreshPlayerMode 
						If (ModeProgress(PlayerMode) = 10) Then
							ModePercent(PlayerMode) = 100
							bModeComplete = True
							pDMDEvent(854) ' Ramps
							setCIULight(False)
						End If
					end if
				Next
			end if
		Case 2:  'same old song   ' random shots, CIU gets all aerosmith blinking for shots
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade
				setCIULight(False)
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1
				bModeProgressUpgraded=True
				If bHardMode Then ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 1000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
				If bHardMode=True Then
					For each a in aerosmithLights
						SetLightColor a, "orange", 2
					Next
				End If
			End If
			D "Looping thru the ramps trigger_name=" & trigger_name 
			b=0
			For each a in aRampLights
	'D "Checking trigger_name=" & trigger_name & " ramp:" & a.name & " state:" & StackState(kStack_Pri0).GetArrowState(a)
				If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
					bDebounce = True	
					D "Match trigger_name=" & trigger_name & " Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)
					' So the ball doesnt roll over twice before we reset
					vpmtimer.addtimer 400, "debounceReset '"
					'PlaySound "gg_RampHit"					' Play sounds and reset the timer to reset the lights
					'LightSeqScore.Play SeqUpOn, 100, 0		 
					PlayProgress trigger_name
					AddScore 15000
					FlashForMs a, 200, 50, 0
					SSetLightColor kStack_Pri0, a, modeRampColor, 0 

					If bModeProgressUpgraded then AddScore 2500  ' some sort of bonus for it being upgraded
					c=getRndShot(b, kStack_Pri0)
					SSetLightColor kStack_Pri0, aRampLights(c), modeRampColor, 2
						
					ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
					Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
					If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then 
						setCIULight(True)
					End If
					ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 10) * 100)  ' 8 shots per this mode
					RefreshPlayerMode 
					If (ModeProgress(PlayerMode) = 10) Then   ' 10 shots  Super Targets
						ModePercent(PlayerMode) = 100
						bModeComplete = True
						pDMDEvent(857)  ' Targets
						setCIULight(False)
					End If
					Exit For
				End If
				b=b+1
			Next
			If bModeProgressUpgraded or bHardMode=False then ' check AEROSMITH Targets
				for each a in aerosmithLights
					If a.name = trigger_name and a.state=2 Then
						SetLightColor a, "orange", 1
						AddScore 10000
						ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1	
						ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 10) * 100)  ' 10 shots per this mode
						RefreshPlayerMode 
						If (ModeProgress(PlayerMode) = 10) Then   ' 10 shots  Super Targets
							ModePercent(PlayerMode) = 100
							bModeComplete = True
							pDMDEvent(857)  ' Targets
						End If
					end If
				Next
			End If
		Case 3:  ' sweet
			D "Looping thru the ramps trigger_name=" & trigger_name 
			For each a in aRampLights
				If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
					bDebounce = True	
					D "Match trigger_name=" & trigger_name & " Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)
					' So the ball doesnt roll over twice before we reset
					vpmtimer.addtimer 400, "debounceReset '"

					PlayProgress trigger_name
					If StackState(kStack_Pri0).GetArrowState(a) = 1 Then  ' If solid then turn off
						AddScore 200000+(15000*Coins)+(15000*ModeProgress(PlayerMode))
						SSetLightColor kStack_Pri0, a, modeRampColor, 0 
					End If 
					If StackState(kStack_Pri0).GetArrowState(a) = 2 Then  ' If blinking then turn off  
						QueueScene "SceneMessage ""1 COIN"","""&FormatScore(500000+(25000*Coins)+(25000*ModeProgress(PlayerMode)))&""","""" '", 500, 2
						QueueScene "SceneClearMessage '", 0, 2
						AddScore 500000+(25000*Coins)+(25000*ModeProgress(PlayerMode))
D "Got Shot"
						SSetLightColor kStack_Pri0, a, modeRampColor, 0 
					End If 
					If StackState(kStack_Pri0).GetArrowState(I35)=0 and _
                          StackState(kStack_Pri0).GetArrowState(I81)=0 and _
                          StackState(kStack_Pri0).GetArrowState(I86)=0 and _
						  StackState(kStack_Pri0).GetArrowState(I91)=0 and _
                          StackState(kStack_Pri0).GetArrowState(I97)=0 and _
                          StackState(kStack_Pri0).GetArrowState(I103)=0 and _
                          ModeProgress(PlayerMode) < 7 and _
										not bModeProgressUpgraded then
						' light hard six
							SSetLightColor kStack_Pri0, I35,modeRampColor,2
							SSetLightColor kStack_Pri0, I62,modeRampColor,2
							SSetLightColor kStack_Pri0, I81,modeRampColor,2
							SSetLightColor kStack_Pri0, I86,modeRampColor,2
							SSetLightColor kStack_Pri0, I97,modeRampColor,2
							SSetLightColor kStack_Pri0, I107,modeRampColor,2

					End if
				
					ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
					Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
					ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 12) * 100)  ' 12 shots per this mode
				'	If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then
				'		setCIULight(True)
				'	End If
					RefreshPlayerMode 
					If (ModeProgress(PlayerMode) = 12) Then   ' 12 shots
						ModePercent(PlayerMode) = 100
						bModeComplete = True
						pDMDEvent(855)  ' Scoring
						setCIULight(False)
					End If
					D "jump out of the for loop"
					Exit For ' Jump out of the For Loop
				end if
			Next
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade
				setCIULight(False)
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1
				bModeProgressUpgraded=True
				If bHardMode Then  ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 3000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
				For each a in aRampLights				' Upgrade to solid
					If StackState(kStack_Pri0).GetArrowState(a) = 0 then
						SSetLightColor kStack_Pri0, a, modeRampColor, 1
					End If
				Next
			End If 
			D "bModeComplete = " & bModeComplete
		Case 4: ' dude
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade - only increase point value
				setCIULight(False)
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1
				bModeProgressUpgraded=True
				If bHardMode Then ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 3000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
			Else ' Move Left shot over to the right, move right shot over to the left
				D "Looping thru the ramps trigger_name=" & trigger_name 
				For each a in aRampLights
					If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
						bDebounce = True	
						D "Match trigger_name=" & trigger_name & " Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)
						' So the ball doesnt roll over twice before we reset
						vpmtimer.addtimer 400, "debounceReset '"

						'PlaySound "gg_RampHit"					' Play sounds and reset the timer to reset the lights
						'LightSeqScore.Play SeqUpOn, 100, 0		 
						PlayProgress trigger_name
						AddScore 15000
						SSetLightColor kStack_Pri0, a, modeRampColor, 0 
						thisScore=200000+(50000*(ModeProgress(PlayerMode)+1))
						If bModeProgressUpgraded then thisScore=thisScore+500000
						Select Case Int(RND()*5)
							Case 0: vidToShow="Video-0x002E.mp4"
							Case 1: vidToShow="Video-0x002F.mp4"
							Case 2: vidToShow="Video-0x0030.mp4"
							Case 3: vidToShow="Video-0x0031.mp4"
							Case Else:   vidToShow="Video-0x000A.mp4"
						End Select
						QueueScene "ScenePlayMessage """&vidToShow&""","""&FormatScore(thisScore)&""","""","""" '", 3000, 2
						QueueScene "SceneClearMessage '", 0, 2
						AddScore thisScore
						If not bModeProgressUpgraded Then 
							If a.name = "I62" Then	
								If StackState(kStack_Pri0).GetArrowState(I81)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I86, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I103, modeRampColor, 1
								End If
							End If
							If a.name = "I81" Then	
								If StackState(kStack_Pri0).GetArrowState(I86)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I91, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I107, modeRampColor, 1
								End If
							End If
							If a.name = "I86" Then	
								If StackState(kStack_Pri0).GetArrowState(I91)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I97, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I62, modeRampColor, 1
								End If
							End If
							If a.name = "I91" Then	
								If StackState(kStack_Pri0).GetArrowState(I97)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I103, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I81, modeRampColor, 1
								End If
							End If
							If a.name = "I97" Then	
								If StackState(kStack_Pri0).GetArrowState(I103)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I107, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I86, modeRampColor, 1
								End If
							End If
							If a.name = "I103" Then	
								If StackState(kStack_Pri0).GetArrowState(I107)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I62, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I91, modeRampColor, 1
								End If
							End If
							If a.name = "I107" Then	
								If StackState(kStack_Pri0).GetArrowState(I62)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I81, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I97, modeRampColor, 1
								End If
							End If
						End If
						ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
						Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
						If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then 
							setCIULight(True)
						End If
						ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 10) * 100)  ' 10 shots per this mode
						RefreshPlayerMode 
						If (ModeProgress(PlayerMode) = 10) Then   ' 10 shots
							ModePercent(PlayerMode) = 100
							bModeComplete = True
							pDMDEvent(858) ' Lanes
							setCIULight(False)
						End If
					end if
				Next
				D "Testing If we are in Upgraded Status :" & bModeProgressUpgraded
				If bModeProgressUpgraded=True and (trigger_name = "sw2" or trigger_name = "sw3" or trigger_name = "sw5") then
					D "Award DUDE"
					ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
					ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 10) * 100)  ' 10 shots per this mode
					RefreshPlayerMode 

					thisScore=200000+(50000*(ModeProgress(PlayerMode)))+500000
					Select Case Int(RND()*5)
						Case 0: vidToShow="Video-0x002E.mp4"
						Case 1: vidToShow="Video-0x002F.mp4"
						Case 2: vidToShow="Video-0x0030.mp4"
						Case 3: vidToShow="Video-0x0031.mp4"
						Case Else:   vidToShow="Video-0x000A.mp4"
					End Select
						QueueScene "ScenePlayMessage """&vidToShow&""","""&FormatScore(thisScore)&""","""","""" '", 3000, 2
						QueueScene "SceneClearMessage '", 0, 2
						AddScore thisScore


					If (ModeProgress(PlayerMode) = 10) Then   ' 10 shots
						ModePercent(PlayerMode) = 100
						bModeComplete = True
						pDMDEvent(858) ' Lanes
						setCIULight(False)
					End If 
				End If
			End If 
		Case 5:  ' back
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade
				setCIULight(False)
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1
				bModeProgressUpgraded=True
				If bHardMode Then ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 3000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
				For each a in aRampLights				' Upgrade to solid
					If StackState(kStack_Pri0).GetArrowState(a) = 0 then
						SSetLightColor kStack_Pri0, a, modeRampColor, 1
					End If
				Next
			Else ' after right orbit, light CIU, each bumper hit moves it one more .. need this to relight the right orbit
				D "Looping thru the ramps trigger_name=" & trigger_name 
				For each a in aRampLights
					If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
						bDebounce = True	
						D "Match trigger_name=" & trigger_name & " Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)
						' So the ball doesnt roll over twice before we reset
						vpmtimer.addtimer 400, "debounceReset '"

'back1						'PlaySound "gg_RampHit"					' Play sounds and reset the timer to reset the lights
						'LightSeqScore.Play SeqUpOn, 100, 0		 
						PlayProgress trigger_name
						bValidHit=True
						AddScore 15000
						SSetLightColor kStack_Pri0, a, modeRampColor, 0 
						If a.name = "I107" and not bModeProgressUpgraded Then   ' light CIU shot then bumpers will move it
							SSetLightColor kStack_Pri0, I35, modeRampColor, 1
							Exit For
						elseIf not bModeProgressUpgraded Then ' turn the orbit back on
							SSetLightColor kStack_Pri0, I107, modeRampColor, 1
							Exit For
						End If 
					End If
				Next
				If bValidHit Then
					ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
					Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
					If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then 
						setCIULight(True)
					End If
					ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 8) * 100)  ' 8 shots per this mode
					RefreshPlayerMode 
					If (ModeProgress(PlayerMode) = 8) Then   ' 8 shots
						ModePercent(PlayerMode) = 100
						bModeComplete = True
						pDMDEvent(853)  ' Pops
						setCIULight(False)
					End If
				end if
			End If 
		Case 6: ' Rats
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade
				setCIULight(False)
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1
				bModeProgressUpgraded=True
				If bHardMode Then ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 3000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
				For each a in aRampLights				' Upgrade to solid
					If StackState(kStack_Pri0).GetArrowState(a) = 0 then
						SSetLightColor kStack_Pri0, a, modeRampColor, 1
					End If
				Next
			Else
				D "Looping thru the ramps trigger_name=" & trigger_name 
				For each a in aRampLights
					If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
						bDebounce = True	
						D "Match trigger_name=" & trigger_name & " Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)
						' So the ball doesnt roll over twice before we reset
						vpmtimer.addtimer 400, "debounceReset '"

						'PlaySound "gg_RampHit"					' Play sounds and reset the timer to reset the lights
						'LightSeqScore.Play SeqUpOn, 100, 0		 
						PlayProgress trigger_name
						
						If StackState(kStack_Pri0).GetArrowState(a) = 1 Then  ' If solid then turn off
							AddScore 15000
							FlashForMs a, 200, 50, 0
							SSetLightColor kStack_Pri0, a, modeRampColor, 0 
						End If 
						If StackState(kStack_Pri0).GetArrowState(I86)=0 and StackState(kStack_Pri0).GetArrowState(I103)=0 and not bModeProgressUpgraded then
							' relight them
							D "SSetLightColorName kStack_Pri0, ""I86"", """ & modeRampColor & """, 1 '"  ' 
							vpmtimer.addtimer 200, "SSetLightColorName kStack_Pri0, ""I86"", """ & modeRampColor & """, 1 '"  ' delay 
							vpmtimer.addtimer 200, "SSetLightColorName kStack_Pri0, ""I103"",""" & modeRampColor & """, 1 '"  ' delay
						End if

						If StackState(kStack_Pri0).GetArrowState(a) = 2 Then  ' If blink then quick blink and return to blink
							AddScore 25000
							If bModeProgressUpgraded Then
								FlashForMs a, 200, 50, 0
							Else
								vpmtimer.addtimer 100,  a.name & ".blinkinterval=20 '"
								vpmtimer.addtimer 1000, a.name & ".blinkinterval=" & BlinkIntDef & " '"
							End If
						End If
						
						ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
						Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
						If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then 
							setCIULight(True)
						End If
						ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 8) * 100)  ' 8 shots per this mode
						RefreshPlayerMode 
						If (ModeProgress(PlayerMode) = 8) Then   ' 8 shots
							ModePercent(PlayerMode) = 100
							bModeComplete = True
							pDMDEvent(856) ' Spinners
							setCIULight(False)
						End If
					end if
				Next
			End If 
		End Select
		If bModeComplete Then
			ModeCountdownTimer.Enabled = False	' Stop the countdown immediatly 
			'PuPlayer.LabelSet pBackglass,"Time", " ",0,""					' leave timer up as we go into mode2
			ShowPlayerModeComplete(-1)			' Show Mode total
			'Finished the level 
			bBonusMode = True
			'not till shot is obtained  setModeSelectLight(True)
			StopPlayerMode
			'''playmedia "Video-0x0000.mp4", "PupVideos", pBonusScreen, "", -1, "Mode Done", 1, 1
		End If
	end if
	CheckSmartButton False
End Sub

Sub CheckModeProgress2nd(trigger_name)
	dim bValidHit
	dim a, b, i 
	dim hitLight 
	Dim bModeComplete

	D "CheckModeProgress2nd " & trigger_name & " playermode:" & playermode & " 2:" & playermode2

	bModeComplete = False
	bValidHit = False

	If PlayerMode <> -1 Then		' Just in Case we get back in here we bail because we are already 100% progress
		D "Mode Progress: " & ModePercent(PlayerMode)
		If Mode2Percent(PlayerMode) >= 100 then 
			D "WE SHOULDNT GET HERE.  Skip Score: " & ModePercent(PlayerMode)
			Exit Sub 
		End If 
	End If

	If bWizardMode then Exit Sub ' Dont score and bonus during Wizard Mode

	Select Case PlayerMode2
		Case -1:' No Mode Selected
		Case 0: ' orbits
			If trigger_name = "I62" or trigger_name = "I107" Then
				PlayProgress trigger_name

				Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
				Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 10) * 100)
				AddScore Mode2Value(PlayerMode2)			' 

				ShowPlayerMode2(PlayerMode2)
				If (Mode2Progress(PlayerMode2) >= 10) Then
					Mode2Percent(PlayerMode2) = 100
					bModeComplete=True
				End If
			End If
		Case 1: ' ramps
			If trigger_name = "I86" or trigger_name = "I103" Then
				PlayProgress trigger_name

				Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
				Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 10) * 100)
				AddScore Mode2Value(PlayerMode2)		' 

				ShowPlayerMode2(PlayerMode2)
				If (Mode2Progress(PlayerMode2) >= 10) Then
					Mode2Percent(PlayerMode2) = 100
					bModeComplete=True
				End If
			End If
		Case 2: ' targets
			For each a in aerosmithLights
				If a.name = trigger_name Then
					SetLightColor a, "orange", 1
					PlayProgress trigger_name

					Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
					Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 20) * 100)
					AddScore Mode2Value(PlayerMode2)		' 

					ShowPlayerMode2(PlayerMode2)
					If (Mode2Progress(PlayerMode2) >= 20) Then
						Mode2Percent(PlayerMode2) = 100
						bModeComplete=True
					End If
					exit for
				End If
			Next
		Case 3: ' scoring
			If trigger_name = "switch" Then
				PlayProgress trigger_name

				Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
				Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 20) * 100)

				ShowPlayerMode2(PlayerMode2)
				If (Mode2Progress(PlayerMode2) >= 20) Then
					Mode2Percent(PlayerMode2) = 100
					bModeComplete=True
				End If
			End If
		Case 4: ' lanes
			If trigger_name = "sw2" or trigger_name = "sw3" or trigger_name = "sw5" Then
				PlayProgress trigger_name

				Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
				Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 10) * 100)
				AddScore Mode2Value(PlayerMode2)	' 

				ShowPlayerMode2(PlayerMode2)
				If (Mode2Progress(PlayerMode2) >= 10) Then
					Mode2Percent(PlayerMode2) = 100
					bModeComplete=True
				End If
			End If
		Case 5: ' pops
			If trigger_name = "bumper" Then
				PlayProgress trigger_name

				Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
				Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 50) * 100)
				AddScore Mode2Value(PlayerMode2)

				ShowPlayerMode2(PlayerMode2)
				If (Mode2Progress(PlayerMode2) >= 50) Then
					Mode2Percent(PlayerMode2) = 100
					bModeComplete=True
				End If
			End If
		Case 6: ' spinner
			If trigger_name = "sw38" Then
				PlayProgress trigger_name

				Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
				Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 100) * 100)
				AddScore Mode2Value(PlayerMode2)			' 

				ShowPlayerMode2(PlayerMode2)
				If (Mode2Progress(PlayerMode2) >= 100) Then
					Mode2Percent(PlayerMode2) = 100
					bModeComplete=True
				End If
			End If
	End Select

	If bModeComplete then
		D "2nd Mode complete" 
		'NOT NEEDED ShowPlayerModeComplete(-1)			' Show 2nd Mode total
		StopPlayerMode2						' Stop this mode.
	End If 
End Sub

Sub StopPlayerMode2
	Dim a
	D "StopPlayerMode2 " & PlayerMode & " 2:" & PlayerMode2
	If bSecondMode then
		StopPlayerModeVideo
		StackState(kStack_Pri0).Disable
	End If

	bSecondMode = False
	If playermode2 = 2 Then
		for each a in aerosmithLights
			SetLightColor a, "orange", 0
		next
	End If
	playermode2 = -1  ' test feb 16

	' turn off pedals and bubble, light chg song
	PuPlayer.LabelSet pBackglass,"Time", " ",1,""
	' RefreshPlayerMode() asdf
	PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress8-0.png",0,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
	pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1)))
	D "StopPlayerMode2 calling SetModeLights"
	SetModeLights

	CheckWizardModesReady	' See of we need to enable wizard modes 
End Sub

Sub PlayProgress(name)
	PlaySoundVol "Metal_Touch_1", VolDef

	If name = "switch" then 
		FlashLevel(10) = 1 : FlasherFlash10_Timer
	Else
		FlashLevel(3) = 1 : FlasherFlash3_Timer
	End If 

End Sub


Function getRndShot(a, Stack_Pri)
Dim b,c
	D "getRndShot " & a
	For b = 0 to 6
		c=Int(7*Rnd())  
		If c<>a and StackState(Stack_Pri).ArrowStates(c).ArrowState = 0 Then Exit For 
	Next
	D "Found shot " & c
	getRndShot=c
End Function


Sub SetModeLightComplete(Idx, Light)
	Dim bUpdated:bUpdated = False
	D "SetModeLightComplete Idx:" & Idx & " %:" & ModePercent(Idx)

	If ModePercent(Idx) >= 0 Then 	' flashes slow
		bUpdated=True
		Light.State = 2
		If bHardMode=True or Idx=7 or Idx=8 Then
			If ModePercent(Idx) > 0 then  ' Need to have completed atleast 1 shot
				modesStarted=modesStarted+1
			End If
		Else
			modesStarted=modesStarted+1
		End If
		Light.BlinkInterval=BlinkIntSlow
		Light.BlinkPattern=BlinkPatternSlow
	End If 
	If ModePercent(Idx) >= 100 Then	' 100% Is Solid
		bUpdated=True
		Light.State = 1
		modesCompleted=modesCompleted+1
		Light.BlinkInterval=BlinkIntDef
		Light.BlinkPattern=BlinkPatternDef
	End If

	If Idx = PlayerMode then 	' Current mode flashes
		bUpdated=True
		Light.State = 2
		Light.BlinkInterval=BlinkIntDef
		Light.BlinkPattern=BlinkPatternDef
	End If 

	If bUpdated=False Then 
		Light.BlinkInterval=BlinkIntDef
		Light.BlinkPattern=BlinkPatternDef		
	End If 
End Sub

dim tmrFinishModeBlinkCnt
Dim tmrFinishModeIndex

Sub SceneFinishMode(Mode)
	dim i
	Dim visible
	If Mode = -1 then exit sub ' Just in Case 
	' Overlay the final onto a video for background
	playmedia "ARS107-Scene-72.mp4", "PupVideos", pOverVid, "", -1, "", 1, 1
	PuPlayer.LabelShowPage pOverVid, 3,0,""
	D "SceneFinishMode2"    	
	For i = 0 to 8
		If i = 7 then 
			If I22.state = 0 then visible = 0 else visible =1
		elseIf i = 8 then 
			If I23.state = 0 then visible = 0 else visible =1
		else
			If ModePercent(i) >= 100 then ' Only those that are COMPLETE
				visible = 1
			else
				visible = 0  ' TODO DEBUG sb 0
			End If 
		End If 
		PuPlayer.LabelSet pOverVid, "F" & i, "PuPOverlays\\CompleteLevel-"&i&".png", visible,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	Next 
'	tmrFinishMode.UserValue=-1
'	tmrFinishModeIndex=Mode
'	tmrFinishMode.Interval = 50
'	tmrFinishMode.enabled = true
End Sub 


Sub SceneClearFinishMode()
	tmrFinishMode.enabled = False
	D "Scene Clear"
	PuPlayer.LabelSet pOverVid, "F0", "PuPOverlays\\CompleteLevel-0.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F1", "PuPOverlays\\CompleteLevel-1.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F2", "PuPOverlays\\CompleteLevel-2.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F3", "PuPOverlays\\CompleteLevel-3.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F4", "PuPOverlays\\CompleteLevel-4.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F5", "PuPOverlays\\CompleteLevel-5.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F6", "PuPOverlays\\CompleteLevel-6.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F7", "PuPOverlays\\CompleteLevel-7.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F8", "PuPOverlays\\CompleteLevel-8.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"

	PuPlayer.LabelShowPage pOverVid, 1,0,""
    playclear pOverVid
End Sub


'sub toyboxmb_end
Function ToyBoxMB_End(checkRelease) ' big TODO
	EndToyBoxMB=False
	If bToyBoxMultiball then 
		D "check Release: " & checkRelease & " pfBalls:" & BallsOnPlayfield & " lock:" &  RealBallsInLock & " MBBonus:" & tmrToyBoxMBBonus.Enabled
		' Release saved balls    ' todo this might need to be realballsinlock
		If BallsOnPlayfield=0 and RealBallsInLock > 0 and tmrToyBoxMBBonus.Enabled then			
																		' During MB If we lock any balls we let them go once we drain (If we are still in MB)
			D "ToyBox MB Release " & RealBallsInLock
			bBallSaverReady = True		' Turn On Ball Saver 
			EnableBallSaver BallSaverTime
			EndToyBoxMB=True
			If RealBallsInLock = 1 and checkRelease then	' Go ahead and EndToyBoxMB but keep playing
				checkRelease = False
			End If
			for i = 0 to RealBallsInLock - 1
				RealBallsInLock = RealBallsInLock -1
				I "ToyBoxMB_End: Releasing 1 Realball Remaining:" 
				I RealBallsInLock
				vpmtimer.addtimer 1000 + (i*200), "CreateNewBallToyBox() '"
			Next
		elseIf BallsOnPlayfield=1 and RealBallsInLock > 0 then 	' We stay in MB because when the ball releases normally we can play MB
			EndToyBoxMB=True
		End If
		If checkRelease then Exit Function 				' Bail out because we are just checking If we continue 

		D "Ending ToyBoxMB"
		dim a, i
		Dim lamp
		StackState(kStack_Pri1).Disable
		aRampLightsRestore				' Put light back to what they should befor mode (Hopefully)
		D "Turn on Elevator Lights " & SaveI94 & SaveI65 & SaveI110
		SetLightColor I94, "white", SaveI94
		SetLightColor I65, "white", SaveI65
		SetLightColor I110,"white", SaveI110
		ShowPlayerModeComplete(0)		' 
		bToyBoxMultiball = False

		PuPlayer.playlistplayex pMusic, "audioclear", "clear.mp3", 100,1
		D "ToyBoxMB End .. prior song?" & PlayerMode
		PlaySong "Song-" & SaveMode & ".mp3"

		ToyBoxMBLocks = 0
		tmrToyBoxMBBonus.Enabled=False	' Disable the timer but leave whatever balls in there
		If bUsePUPDMD then puPlayer.LabelSet pBackglass,"MTimer",	""	,1,""
		bToyBoxBonus3x=False
		bAutoPlunger=False
		CheckWizardModesReady
	End If 
End Function

Sub CreateNewBallToyBox   'todo dump from toybox
    sw61.CreateSizedball BallSize / 2

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
	sw61.Kick -36, 7+RndNum(0,5)
' If there is 2 or more balls then set the multiball flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield-realballsinlock > 1 Then
		D "TOYBOX SETTING MULTIBALL MODE " & BallsOnPlayfield & "!!!!!!!!!!!!!!!!!!"
        bMultiBallMode = True
    End If
End Sub


Sub debounceReset() 		' Reset Debounce for those that dont have a timer
	bDebounce = False
End Sub

const kDMD_Myst_IsLit		=599		
const kDMD_Myst_AddABall	=600
const kDMD_Myst_AddTime 	=601
const kDMD_Myst_10Million 	=602
const kDMD_Myst_2Million	=603		
const kDMD_Myst_20Million	=604
const kDMD_Myst_BonusHold 	=605
const kDMD_Myst_BonusXHold	=606
const kDMD_Myst_DoubleScore =607
const kDMD_Myst_Lock1		=608
const kDMD_Myst_Lock2    	=609		
const kDMD_Myst_Lock3       =610
const kDMD_Myst_ExtraBallLit=611
const kDMD_Myst_HPhoneHurry =612
const kDMD_Myst_IncreasePop =613		
const kDMD_Myst_IncreaseSpin=614
const kDMD_Myst_ShotMult	=615
const kDMD_Myst_SmartMiss	=616
const kDMD_Myst_SmartMissLit=617		
const kDMD_Myst_SpecialLit	=618		
const kDMD_Myst_SuperScore	=619	
const kDMD_Myst_SmartMissQ  =620

const kDMD_Myst_MAX			=621

const kDMD_Myst_BonusMultiplier=621
const kDMD_Myst_Bonus2x		=622
const kDMD_Myst_Bonus3x		=623		
const kDMD_Myst_Bonus4x		=624		
const kDMD_Myst_Bonus5x		=625		
const kDMD_Myst_Bonus6x		=626		
const kDMD_Myst_Bonus7x		=627
const kDMD_Myst_Bonus8x		=628		
const kDMD_Myst_Bonus9x		=629		
const kDMD_Myst_Bonus10x	=630		

Dim bToyBoxMultiball:bToyBoxMultiball=False
Sub AwardMystery()
	dim award 
	award = INT((kDMD_Myst_MAX-600+1)*Rnd+600) 
	D "Award " + cstr(award)
	If bExtraBallWonThisBall Then
		If award = kDMD_Myst_ExtraBallLit or award = kDMD_Myst_AddABall Then
			award = kDMD_Myst_2Million
		End If
	End If

	If (bSecondMode or PlayerMode = -1) and award = kDMD_Myst_AddABall Then
		award=kDMD_Myst_2Million
	End If

	If (bElevMultiBall or bToyBoxMultiball) Then
		If award = kDMD_Myst_Lock1 or award = kDMD_Myst_Lock2 or award = kDMD_Myst_Lock3 then
			award = kDMD_Myst_2Million
		End If
	End If
	If (bElevMultiBall or bToyBoxMultiball) and not bExtraBallWonThisBall Then
		award = kDMD_Myst_AddABall 
		bExtraBallWonThisBall=True
	End If 
	If award = kDMD_Myst_AddTime and ModecountdownTimer.UserValue <=0 Then
		award = kDMD_Myst_2Million
	End If

	If award <> kDMD_Myst_BonusMultiplier then		' This one is based off the current multiplier 
		pDMDEvent(award)
	End If 

	Select Case award:
		Case kDMD_Myst_BonusMultiplier:
			If BonusMultiplier < 10 then
				AddBonusMultiplier 1
			Else
				DisplayDMDText2 "MYSTERY","2 MILLION", 1000, 11, 0
				AddScore 2000000
			End If
		Case kDMD_Myst_AddTime:
			DisplayDMDText2 "MYSTERY","AWARD MORE TIME", 1000, 11, 0
			ModecountdownTimer.UserValue=ModecountdownTimer.UserValue+20
		Case kDMD_Myst_SuperScore:
			DisplayDMDText2 "MYSTERY","SUPER SCORING", 1000, 11, 0
		Case kDMD_Myst_BonusXHold:
			DisplayDMDText2 "MYSTERY","BONUS X HOLD", 1000, 11, 0
		Case kDMD_Myst_DoubleScore:
			DisplayDMDText2 "MYSTERY","DOUBLE SCORING", 1000, 11, 0
		Case kDMD_Myst_IncreasePop:
			DisplayDMDText2 "MYSTERY","INC POP VALUE", 1000, 11, 0
			PopValue=int(PopValue*1.15)
			BumperMultiplier=BumperMultiplier+1
			PopHits=25
			PopLevel=PopLevel+1
		Case kDMD_Myst_IncreaseSpin:
			DisplayDMDText2 "MYSTERY","INC SPIN VALUE", 1000, 11, 0
		Case kDMD_Myst_ShotMult:
			DisplayDMDText2 "MYSTERY","SHOT MULTIPLIER", 1000, 11, 0
		Case kDMD_Myst_SmartMiss:
			DisplayDMDText2 "MYSTERY","SMART MISSLE", 1000, 11, 0
			SmartButtonCount=SmartButtonCount+1  ' Give you one then call the routine
			SmartMissilePressed()
		Case kDMD_Myst_SmartMissLit:
			DisplayDMDText2 "MYSTERY","SMART MISSILE LIT", 1000, 11, 0
			SmartButtonCount=SmartButtonCount+1
		Case kDMD_Myst_SmartMissQ:
			DisplayDMDText2 "MYSTERY","SMART MISSILE QUALIFIED", 1000, 11, 0
			SmartButtonCount=SmartButtonCount+1  ' You get it but cant use it now anyways
		Case kDMD_Myst_SuperScore:
			DisplayDMDText2 "MYSTERY","SUPER SCORING", 1000, 11, 0
		Case kDMD_Myst_Lock1,kDMD_Myst_Lock2,kDMD_Myst_Lock3:
			ElevMultiBallCount = ElevMultiBallCount + 1
			'vpmtimer.addtimer 4000, "pDMDEvent(" & kDMD_ElevCollected + ElevMultiBallCount -1 & ") '"   ' TODO the elevator anim.
			DisplayDMDText2 "MYSTERY","LOCK AWARDED", 1000, 11, 0
			Select Case ElevMultiBallCount:
				Case 1:
					QueueScene "ScenePlayMessage ""Elevator.mp4"", ""BALL 1"",""LOCKED"","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Case 2:
					QueueScene "ScenePlayMessage ""Elevator.mp4"", ""BALL 2"",""LOCKED"","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Case 3:
					QueueScene "ScenePlayMessage ""Elevator.mp4"", ""BALL 3"",""LOCKED"","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
			End Select
		Case kDMD_Myst_IsLit:
			DisplayDMDText2 "MYSTERY","VIP IS LIT", 1000, 11, 0
			SetLightColor I27, "white", 2
		Case kDMD_Myst_SpecialLit:
			DisplayDMDText2 "MYSTERY","SPECIAL IS LIT", 1000, 11, 0
			SetLightCOlor I11, "red", 2
		Case kDMD_Myst_AddABall:	' Add Multiball
			DisplayDMDText2 "MYSTERY","Add A Ball", 1000, 11, 0	
			I "Mystery add"
			AddMultiball 1
		Case kDMD_Myst_BonusHold:	
			DisplayDMDText2 "MYSTERY","BONUS HOLD", 1000, 11, 0
		Case kDMD_Myst_HPhoneHurry:
			DisplayDMDText2 "MYSTERY","HURRYUP", 1000, 11, 0
		Case kDMD_Myst_2Million:
			DisplayDMDText2 "MYSTERY","2 MILLION", 1000, 11, 0
			AddScore 2000000
		Case kDMD_Myst_10Million:
			DisplayDMDText2 "MYSTERY","10 MILLION", 1000, 11, 0
			AddScore 10000000
		Case kDMD_Myst_20Million:
			DisplayDMDText2 "MYSTERY","20 MILLION", 1000, 11, 0
			AddScore 20000000
		Case kDMD_Myst_ExtraBallLit:
			setExtraBallLight(True)
			DisplayDMDText2 "MYSTERY","EXTRA BALL LIT", 1000, 11, 0
		Case Else:
			DisplayDMDText2 "MYSTERY","2 MILLION", 1000, 11, 0
			AddScore 2000000
	End Select 
End Sub

Sub SmartMissilePressed
	D "SmartMissilePressed"
End Sub

Sub debug_same
' settimer to 1
	ModeProgress(2)=9
	ModePercent(2) = CINT((ModeProgress(2)  / 10) * 100)
	RefreshPlayerMode
	CheckModeProgress("I42")
End Sub

Sub debug_walk
' settimer to 1
	ModeProgress(1)=5
	ModePercent(1) = CINT((ModeProgress(1)  / 10) * 100)
	RefreshPlayerMode
	CheckModeProgress("I42")
End Sub

sub debug_medley
dim a
ModecountdownTimer.UserValue = int(ModecountdownTimer.UserValue/3)
	for a = 0 to 8
		ModeProgress(a)=5
		ModePercent(a)=50
	Next 
	ModeOrder(0)=4
	ModeOrder(1)=3:ModePercent(3)=100
	ModeOrder(2)=6:ModePercent(6)=100
	ModeOrder(3)=8:ModePercent(8)=100
	ModeOrder(4)=1:ModePercent(1)=100
	ModeOrder(5)=2:ModePercent(2)=100
	ModeOrder(6)=0:ModePercent(0)=100
	ModeOrder(7)=5:ModePercent(5)=100
	ModeOrder(8)=7
	SetModeLights
end sub
