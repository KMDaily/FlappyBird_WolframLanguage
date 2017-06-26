(* ::Package:: *)

(* ::Section:: *)
(*Compiled functions*)


(* ::Subsection::Closed:: *)
(*updatePosVel*)


(* ::Text:: *)
(*updates position and velocity based on gravity, fps (frames-per-second), and kick values*)
(*returns {position, velocity}*)


(* ::Input::Initialization:: *)
updatePosVel=Compile[{{pos,_Real},{vel,_Real},{fps,_Real},{gravity,_Real},{kick,_Real}},
Module[{velC=vel,posC=pos},
velC-=gravity/fps/fps;
posC+=0.5*velC/fps;

(* if Flappy is at or below the ground, stop forward velocity and keep Flappy on the ground *)
If[posC<2.5,posC=2.5;velC=0];

{posC,velC}]
];


(* ::Subsection::Closed:: *)
(*createPipe*)


(* ::Text:: *)
(*creates a set of pipes off of the right-hand-side of the screen*)


(* ::Input::Initialization:: *)
createPipe=Compile[{{height,_Real},{gap,_Real},{width,_Real},{pos,_Real}},
{
{{pos,0},{pos,height},{width+pos,height},{width+pos,0},{pos,0}},
{{pos,height+gap},{pos,20+height},{width+pos,20+height},{width+pos,height+gap},{pos,height+gap}}
}
];


(* ::Subsection::Closed:: *)
(*updatePipe*)


(* ::Text:: *)
(*removes pipe from left-hand-side of screen*)
(*creates new random pipe on right-hand-side of screen*)


(* ::Input::Initialization:: *)
updatePipe=Compile[{{pipeList,_Real,4},{pipeSpacing,_Real},{pipeWidth,_Real}},
Module[{p=pipeList},
Do[
If[
Max[p[[i,1,All,1]]]<=0,
p[[i]]=createPipe[RandomReal[{2.5,10}],pipeSpacing,pipeWidth,14.]
],{i,1,Length[p]}];
p
]
];


(* ::Subsection::Closed:: *)
(*myClip*)


(* ::Text:: *)
(*compiled version of the Clip function assuming real values*)


(* ::Input::Initialization:: *)
myClip=Compile[{{p,_Real},{m,_Real,1}},
If[p<m[[1]],m[[1]],
If[p>m[[2]],m[[2]],p]
]
];


(* ::Subsection::Closed:: *)
(*distToRectangle*)


(* ::Text:: *)
(*shortest distance from a rectangle to a point*)


(* ::Input::Initialization:: *)
distToRectangle=Compile[{{r,_Real,2},{p,_Real,1}},
\[Sqrt]((-myClip[p[[1]],{r[[1,1]],r[[2,1]]}]+p[[1]])^2+(-myClip[p[[2]],{r[[1,2]],r[[2,2]]}]+p[[2]])^2)
];


(* ::Subsection::Closed:: *)
(*collision*)


(* ::Text:: *)
(*calculates distances from point to all rectangles*)


(* ::Input::Initialization:: *)
collision=Compile[{{pipeList,_Real,4},{point,_Real,1}},
Module[{pList,p2},
pList=Transpose[MinMax/@Transpose[#]]&/@Flatten[pipeList,1];
distToRectangle[#,point]&/@pList
]
];


(* ::Section:: *)
(*graphics*)


(* ::Subsection::Closed:: *)
(*static graphics*)


frame1 = Import["Sprites/frame1.png"];
frame2 = Import["Sprites/frame2.png"];
frame3 = Import["Sprites/frame3.png"];
backgroundColor = RGBColor@@ImageData[frame1][[1,1]];
gifSequence = RemoveBackground /@ {frame1,frame2,frame3,frame2};
invertPipeImage = Import["Sprites/pipe.PNG"];
pipeImage = ImageReflect[invertPipeImage];
ground = Import["Sprites/ground.PNG"];
city = Import["Sprites/city.PNG"];
digits = RemoveBackground /@ First[ImagePartition[ImageCrop[ImageTake[Import["Sprites/digits.jpg"],{100,250}]],{42.76,60.2}]];
zero = ImageCrop[Import["Sprites/zero.png"]];
digits = Join[digits,{zero}];


(* ::Subsection::Closed:: *)
(*calculates score in Flappy number graphics*)


(* ::Input::Initialization:: *)
makeScore[n_Integer]:=Row[digits[[IntegerDigits[n]/.(0->10)]]]


(* ::Section:: *)
(*sounds*)


(* ::Subsection::Closed:: *)
(*sound effects*)


flapSound = Import["Sounds/flapSound.wav", "Sound"];
hurtSound = Import["Sounds/hurtSound.wav", "Sound"];
scoreSound = Import["Sounds/scoreSound.wav", "Sound"];


SetDirectory[];


(* ::Section:: *)
(*game*)


(* ::Input::Initialization:: *)
playFlappyBird[]:=DynamicModule[{task,pos=0,vel=0,fps=45,gravity=2500,kick=600,pipeVel=4,pipeSpacing=3.4,pipeWidth=2.,pipeList={},previousButtonState=False,gif=1,bird,groundPosition=-2,scoreValue=0,readyToScore=True,startState=False,hurtSoundSwitch=False,instructions},
	
(* graphics, most with dynamic positions *)
Button[
Overlay[{
Graphics[{
(* pipes *)
Inset[invertPipeImage,Dynamic[First[{pipeList[[1,2,1]]}]],Scaled[{0,0}],2.05],
Inset[pipeImage,Dynamic[First[{pipeList[[1,1,2]]}]],Scaled[{0,1}],2.05],
Inset[invertPipeImage,Dynamic[First[{pipeList[[2,2,1]]}]],Scaled[{0,0}],2.05],
Inset[pipeImage,Dynamic[First[{pipeList[[2,1,2]]}]],Scaled[{0,1}],2.05],

(* Flappy *)
Inset[Dynamic[First[{bird}]],{5,Dynamic[First[{pos}]]},Scaled[{0.5,0.5}],1.325],

(* ground *)
Inset[ground,{Dynamic[First[{groundPosition}]],-2},Scaled[{0,0}],43.7]
},
Frame->False,
PlotRange->{{0,10},{0,14}},
ImageSize->350,
Background->RGBColor[0.44313725490196076`,0.7803921568627451`,0.8156862745098039`],
Prolog->Inset[city,{-0.07,1.88},Scaled[{0,0}],10.08]
],

(* score is an overlay over the main graphic to help with formatting *)
Dynamic[makeScore[scoreValue]],

Dynamic[instructions]
},
Alignment->{0,0.6}
],
instructions=Invisible[instructions];StartScheduledTask[task],
Appearance->"Frameless"
]
,

Initialization:>( 
(* display instructions *)
instructions = Framed[Style[Column[{"Click to start.","Press Control Key to flap."},Alignment->Center],24,Bold,Red],RoundingRadius->5,FrameStyle->Thick,Background->White];

(* pipes initialized off the right side of the screen, spaced apart to match 1/2 the view window*)
pipeList={
createPipe[RandomReal[{2.5,10}],pipeSpacing,pipeWidth,15.],
createPipe[RandomReal[{2.5,10}],pipeSpacing,pipeWidth,22.5]
};

(* position Flappy halfway up, starting on first animation frame *)
pos=7;
bird=gifSequence[[gif]];

(* initialize score to 0 *)
score=makeScore[scoreValue];

(* create scheduled task to run game *)
task=CreateScheduledTask[
Switch[{previousButtonState,CurrentValue["ControlKey"]},
{False,True},previousButtonState=True;EmitSound[flapSound];vel=kick/fps,
{True,False},previousButtonState=False,
_,Null
];

(* animate bird and ground if there is forward velocity *) 
If[pipeVel>0,
gif=1+Mod[++gif,4];
bird=gifSequence[[gif]];
groundPosition-=pipeVel/fps;
];
If[groundPosition<-10,groundPosition=-2];

(* update bird position *)
{pos,vel}=updatePosVel[pos,vel,fps,gravity,kick];

(* update all existing pipes *)
pipeList[[All,All,All,1]]-=pipeVel/fps;

(* increase score *)
If[Apply[And,#>5.&/@Min/@pipeList[[All,All,All,1]]],readyToScore=True];
If[Min[Max/@pipeList[[All,All,All,1]]]<5&&readyToScore,
EmitSound[scoreSound];
scoreValue+=1;
readyToScore=False;
];

(* if pipe moves off screen, remake it randomly on the right *)
pipeList=updatePipe[pipeList,pipeSpacing,pipeWidth];

(* check for collisions *)
If[AnyTrue[collision[pipeList,{5,pos}],#<0.5&]||pos<=2.5,
If[!hurtSoundSwitch,EmitSound[hurtSound];hurtSoundSwitch=True];
previousButtonState=Null;
pipeVel=0;
];

(* stop scheduled task if Flappy falls to the ground *)
If[pos <= 2.5, RemoveScheduledTask[ScheduledTasks[]]];
,
1./fps
]
)
]
