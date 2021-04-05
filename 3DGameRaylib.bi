'' needed headers
'#Include Once "ship.bi"
#Include "string.bi"
#include "arraylist.bi"
'#Include Once "object.bas"
'#Include Once "route.bi"
#Include Once "sectors.bi"
#Include Once "crt/stdlib.bi"
'
'Type ColorType
'	Union
'	As Ulong value
'	Type
'		blue As uByte
'		green As uByte
'		red As ubyte
'		a As Ubyte
'	End Type
'	End Union
'End Type
'
'
'
Randomize Timer*1000

Dim Shared line_pattern(0 To 15) As UShort={&b0000000011111111,&b0000000111111110,&b0000001111111100,&b0000011111111000,&b0000111111110000,&b0001111111100000,&b0011111111000000,&b0111111110000000,&b1111111100000000,&b1111111000000001,&b1111110000000011,&b1111100000000111,&b1111000000001111,&b1110000000011111,&b1100000000111111,&b1000000001111111}


'#Include Once "sectors.bi"

'Declare Sub sortList(list As arrayList)
'Declare Sub generate_sectors(n As vec4 Ptr,epoch As Double,nr_sys As uinteger)

Type connection_t
	dist As Double
	connected As Integer
	node_1 As UInteger
	node_2 As UInteger
	Declare Constructor ()
	declare constructor( byval d as Double,n1 As UInteger,n2 As uinteger )
	Declare Destructor ()
End Type
constructor connection_t()
dist = 0
node_1=0
node_2=0
connected=0
End constructor

Constructor connection_t( byval d as Double,n1 As UInteger,n2 As uinteger )
dist = d
node_1=n1
node_2=n2
connected=0
End constructor

Destructor connection_t()
End Destructor
'
'Dim Shared con As connection_t Ptr
'Dim Shared As arrayList connectionList_tmp
'Dim Shared As arrayList connectionList
'
'


'Function get_distance(n1 As node_t, n2 As node_t) As Double
'	Return Sqr( (n2.posi.x-n1.posi.x)*(n2.posi.x-n1.posi.x) + (n2.posi.y-n1.posi.y)*(n2.posi.y-n1.posi.y) + (n2.posi.z-n1.posi.z)*(n2.posi.z-n1.posi.z) )
'End Function
'Function calcConnectionCount(num_nodes As UInteger) As UInteger
'   Return (num_nodes*(num_nodes+1)/2) - num_nodes' See http://de.wikipedia.org/wiki/Der_kleine_Gau%C3%9F
'End Function


Type galaxy_t
	Declare Constructor()
	Declare Constructor(seed As uinteger)
	Declare Constructor(filename As string)
	Declare Destructor()

	Declare Sub set_seed(s As UInteger)
	Declare Sub regenerate()
	Declare Sub regenerate_test()
	Declare Sub generate_MST(cam As camera)
	Declare Sub get_path(s As UInteger, t As uinteger)
	Declare Sub render_model(cam As camera,a As Single)
	Declare Sub pre_render_model(cam As camera)
	Declare Sub render_view_model(cam As camera, a As Single,s As boolean=TRUE)
	Declare Sub render_path(cam As camera)
'	model As object3d Ptr
	'known_systems As object3d Ptr
'	path As object3d Ptr

'	path_nodes As arrayList
	model0 As Model
	mesh0 As Mesh
	nodes As arrayList
	seed As UInteger
	count As UInteger
	sprite As Any Ptr
End Type
Destructor galaxy_t()
'if( model->vertices->count > 0 ) then
'	for i as integer = 0 to model->vertices->count - 1
'		model->vertices->removeLast()
'	next
'end if
'if( known_systems->vertices->count > 0 ) then
'	for i as integer = 0 to known_systems->vertices->count - 1
'		known_systems->vertices->removeLast()
'	next
'end if
'
''' same for the edges
'
'if( model->edges->count > 0 ) then
'	for i as integer = 0 to model->edges->count - 1
'		model->edges->removeLast()
'	next
'end If
'If path_nodes.count()>0 Then
'	For i As UInteger=0 To path_nodes.count()-1
'		path_nodes.removeLast()
'	Next
'EndIf
'path->destructor()
'free(model0.meshes)
'Print "here"
End Destructor
Constructor galaxy_t()

End Constructor
Constructor galaxy_t(seed As uinteger)
'model=New object3d()
'known_systems=New object3d()
'path=New object3d()
count=0
set_seed(seed)
regenerate()
End Constructor

Sub galaxy_t.set_seed(seed As UInteger)
	this.seed=seed
	Randomize seed
End Sub

Sub galaxy_t.regenerate()
	
	Dim As UByte Ptr Ptr Ptr gal'(0 To 255,0 To 64,0 To 255)
	'gal=calloc(256,SizeOf(UByte Ptr ptr))
	'For i As Integer=0 To 255
	'	gal[i]=calloc(64,SizeOf(UByte ptr))
'		For j As Integer=0 To 
	'Next
	''' if there are vertices on the object, release them
	'if( model->vertices->count > 0 ) then
	'	for i as integer = 0 to model->vertices->count - 1
	'		model->vertices->removeLast()
	'	next
	'end if
	'if( known_systems->vertices->count > 0 ) then
	'	for i as integer = 0 to known_systems->vertices->count - 1
	'		known_systems->vertices->removeLast()
	'	next
	'end if

	''' same for the edges

	'if( model->edges->count > 0 ) then
	'	for i as integer = 0 to model->edges->count - 1
	'		model->edges->removeLast()
	'	next
	'end If

	'set_seed(this.seed)

	''Color &hffffffff,&h00
	'Dim x As Double
	'Dim y As Double
	'Dim z As Double
	'Dim nr_arms As UInteger=GetScrollBarVal(ScrollSpiralArms)
	'model->vertices->Add( New vec4( 0,0,0 ))
	'count=1
	'For j As UInteger=0 To 360-360\nr_arms Step 360\nr_arms
	'	'count+=1
	'	For i As uinteger=0 To 1440 Step 1
	'		Dim As Single k=GetScrollBarVal(ScrollSpiralK)/10.0
	'		Dim As Single p=GetScrollBarVal(ScrollSpiralP)/10.0
	'		Dim r1 As single=(((i*i)/(k/p)^4))
	'		'Dim r1 As single=sqr(((i*i)/(k/p)^16))
	'		If r1>=5.0 Then Exit For
	'		y=0
	'		x=0
	'		z=0
	'		'generates a galaxy in parsects
	'		For s As uinteger=0 To sigmoid(map(r1,0,5,0,-16),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )*3 Step nr_arms
	'			Dim p1 As vec4
	'			x=(-0.5+Rnd)*sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )*GetScrollBarVal(ScrollBulgeHeight)/10'
	'			z=(-0.5+Rnd)*sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )*GetScrollBarVal(ScrollBulgeHeight)/10'
	'			y=(-0.5+Rnd)*sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )*GetScrollBarVal(ScrollBulgeHeight)/10
	'			p1.y=(y*3.26156)
	'			p1.x=(Sin((i+j)*degToRad)*r1*15.0*3.26156+x*5.0*3.26156)
	'			p1.z=(Cos((i+j)*degToRad)*r1*15.0*3.26156+z*5.0*3.26156)
	'			'galaxy_tmp->vertices->Add( New vec4( p1.x, p1.y, p1.z ) )
	'			model->vertices->Add( New vec4( p1.x, p1.y, p1.z ))
	'			count+=1
	'			For s1 As uinteger=0 To (sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 ))*3 Step 1
	'				x=(-0.5+Rnd)*sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )*GetScrollBarVal(ScrollBulgeHeight)/10'
	'				z=(-0.5+Rnd)*sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )*GetScrollBarVal(ScrollBulgeHeight)/10'
	'				y=(-0.5+Rnd)*sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )*GetScrollBarVal(ScrollBulgeHeight)/10
	'				p1.y+=(y*2*3.26156)
	'				p1.x+=(x*2*3.26156)
	'				p1.z+=(z*2*3.26156)
	'				model->vertices->Add( New vec4( p1.x, p1.y, p1.z ))
	'				'					drawPoint3d(cam,vec4( p1.x, 0, p1.z ),sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )*320.0,c)
	'				count+=1
	'			Next
	'		Next
	'	Next
	'Next
	''Dim v As vec4 Ptr=model->vertices->Get(Rnd*(model->vertices->count))
	''known_systems->vertices->Add(New vec4(v->x,v->y,v->z))
	''model->vertices->remove(v)
	''v=model->vertices->Get(Rnd*(model->vertices->count))
	''known_systems->vertices->Add(New vec4(v->x,v->y,v->z))
	''For i As UInteger=0 To model->vertices->count-1
	''		Dim n As vec4 Ptr =model->vertices->Get(i)
	''		generate_sectors(n,timer)
	''	Next
End Sub
Sub galaxy_t.generate_MST(cam As camera)
	'If known_systems->vertices->count>2 Then
	'	Color &hffffffff
	'				Print "here"
	'				Sleep 100



	'	Dim vert1_ptr As arrayList Ptr=known_systems->vertices()
	'	Dim vert2_ptr As arrayList Ptr=known_systems->vertices()
	'	Dim con As connection_t Ptr
	'	Dim As arrayList connectionList_tmp
	'	Dim As arrayList connectionList


	'	Dim As UInteger from_counter_max=((known_systems->vertices->count-2))'*(galaxy_tmp->vertices->count)/2)-(galaxy_tmp->vertices->count-1)'(STARS*(STARS+1)/2) - STARS
	'	Dim As UInteger from_counter'=from_counter_max
	'	Dim As UInteger to_counter'=galaxy_tmp->vertices->count-1
	'	Dim dist As Double
	'	Dim dist0 As Double
	'	Dim vert1 As vec4 Ptr
	'	Dim vert2 As vec4 Ptr
	'	Dim As connection_t Ptr con1
	'	Dim id1 As UInteger
	'	Dim id2 As UInteger
	'	Dim counter As uinteger=0

	'	'generate distance array
	'	Print "generate distance array"
	'	For i As UInteger=0 To known_systems->vertices->count()-1
	'		'Locate 1,1
	'		'Print i/galaxy_tmp->vertices->count()*100
	'		vert1 = vert1_ptr->Get(i)
	'		For j As UInteger=i To known_systems->vertices->count()-1
	'			vert2 = vert1_ptr->Get(j)
	'			dist=distance3d(vert2->x,vert2->y,vert2->z,vert1->x,vert1->y,vert1->z)
	'			If i<>j And dist<= (sigmoid(map(dist,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )*GetScrollBarVal(ScrollBulgeHeight)/10)*8 Then'dist<=(GetScrollBarVal(ScrollBulgeHeight)/10+GetScrollBarVal(ScrollBulgeHeight)/100)*2 Then
	'				con=New connection_t(dist,i,j)
	'				connectionList.Add(con)
	'			EndIf
	'		Next
	'	Next
	'	'generate spannbaum
	'	Print "generate MST"
	'	from_counter_max=connectionList.count()-1
	'	sortList(connectionList)
	'	While (to_counter<known_systems->vertices->count-1)
	'		'	sortList(connectionList)
	'		'sort distance array
	'		Dim s As String="Generating MST: " & Format(1-((connectionList.count()/from_counter_max)),"000.00%")
	'		Dim si As UInteger=Len(s)*16
	'		Line(scrW/3,scrH-50)-(scrW/3+si,scrH-50+20),&hff000000,BF
	'		Line(scrW/3,scrH-50)-(scrW/3+map(connectionList.count()/from_counter_max,1.0,0.0,0.0,si),scrH-50+20),&hff3333ff,BF
	'		Line(scrW/3,scrH-50)-(scrW/3+si,scrH-50+20),&hffffffff,B
	'		Draw String(scrW/3+si\4,scrH-50+8),s,&hffffffff
	'		con1=connectionList.get(0)
	'		con1->connected=1
	'		known_systems->edges->add( new edge( vert1_ptr->Get(con1->node_1),vert1_ptr->Get(con1->node_2) ) )
	'		counter=0
	'		id1 =con1->node_1
	'		id2 =con1->node_2
	'		dist=con1->dist
	'		While (counter<connectionList.count()-1)
	'			con1=connectionList.get(counter)
	'			If (con1->node_2=id2) Then 'And (con1->node_2<>id1)Then'Or (con1->node_2=id2)Then'and not(con1->node_1=id2) Then
	'				'			Print "here",to_counter,model->vertices->count-1,counter,from_counter_max
	'				con1->connected=1
	'				connectionList.remove(counter)
	'				counter-=1
	'				'Continue while
	'			EndIf
	'			counter+=1
	'		Wend
	'		to_counter+=1
	'		'render_model(cam)
	'	Wend
	'		'build_nodes(known_systems,0,0)
	'endif

End Sub


Sub galaxy_t.render_model(cam As camera,a As Single)
	'Dim As UInteger cols(0 To 1)={&hff6666ff,&hffff6666}
	'Dim r As UInteger=RGBA(127,127,255,a*64)
	'If known_systems->edges->count>0 Then

	'	dim as edge ptr e = known_systems->edges->get( 0 )
	'	for j as integer = 0 to known_systems->edges->count - 1
	'		e = known_systems->edges->Get( j )
	'		Dim dist1 As Double=distance3d(0,0,0,e->vertex1->x,e->vertex1->y,e->vertex1->z)
	'		Dim dist2 As Double=distance3d(0,0,0,e->vertex2->x,e->vertex2->y,e->vertex2->z)
	'		Dim As UInteger col_s1=cols(Int(density(map(dist1,0,50,-4,4))+0.75))
	'		Dim As UInteger col_s2=cols(Int(density(map(dist2,0,50,-4,4))+0.75))
	'		'Print col_s2,Int(density(map(dist2,0,50,-4,4))+0.5),density(map(dist2,0,50,-4,4))+0.5
	'		'			drawPoint3d(cam,galaxy->objectSpaceToWorldSpace( *e->vertex1 ),10,&h336666ff)

	'		If e->vertex1->x=0 And e->vertex1->y=0 And e->vertex1->z=0 then
	'			'r=Temperature2RGB(Rnd*2000+2000+e->vertex2->distance(vec4(0,0,0))*100,127)
	'			'drawPoint3d(cam,model->objectSpaceToWorldSpace( *e->vertex1 ),10,&hffff0000)
	'			'drawPoint3d(cam,model->objectSpaceToWorldSpace( *e->vertex2 ),10,r)
	'			drawLine3d( cam, known_systems->objectSpaceToWorldSpace( *e->vertex1 ), known_systems->objectSpaceToWorldSpace( *e->vertex2 ), rgba( 255, 00, 00, 255 ),line_pattern(Int((Timer*10.0))Mod 16) )
	'		Else
	'			'drawPoint3d(cam,galaxy->objectSpaceToWorldSpace( *e->vertex1 ),10,col_s1)
	'			'		drawCircle3d(cam,galaxy->objectSpaceToWorldSpace( *e->vertex2 ),1,8,&h336666ff)
	'			'r=Temperature2RGB(Rnd*2000+2000+e->vertex2->distance(vec4(0,0,0))*100,127)
	'			'drawPoint3d(cam,model->objectSpaceToWorldSpace( *e->vertex2 ),10,r)
	'			drawLine3d( cam, known_systems->objectSpaceToWorldSpace( *e->vertex1 ), known_systems->objectSpaceToWorldSpace( *e->vertex2 ), rgba( 64, 64, 255, 255 ) ,line_pattern(Int((Timer*10.0))Mod 16))
	'		endif
	'	next
	'EndIf

	''If known_systems->edges->count>0 Then

	''	dim as edge ptr e = known_systems->edges->get( 0 )
	''	for j as integer = 0 to known_systems->edges->count - 1
	''		e = known_systems->edges->Get( j )
	''		Dim dist1 As Double=distance3d(0,0,0,e->vertex1->x,e->vertex1->y,e->vertex1->z)
	''		Dim dist2 As Double=distance3d(0,0,0,e->vertex2->x,e->vertex2->y,e->vertex2->z)
	''		Dim As UInteger col_s1=cols(Int(density(map(dist1,0,50,-4,4))+0.75))
	''		Dim As UInteger col_s2=cols(Int(density(map(dist2,0,50,-4,4))+0.75))
	''		'Print col_s2,Int(density(map(dist2,0,50,-4,4))+0.5),density(map(dist2,0,50,-4,4))+0.5
	''		'			drawPoint3d(cam,galaxy->objectSpaceToWorldSpace( *e->vertex1 ),10,&h336666ff)

	''		If e->vertex1->x=0 And e->vertex1->y=0 And e->vertex1->z=0 then
	''			'r=Temperature2RGB(Rnd*2000+2000+e->vertex2->distance(vec4(0,0,0))*100,127)
	''			'drawPoint3d(cam,model->objectSpaceToWorldSpace( *e->vertex1 ),10,&hffff0000)
	''			'drawPoint3d(cam,model->objectSpaceToWorldSpace( *e->vertex2 ),10,r)
	''			drawLine3d( cam, model->objectSpaceToWorldSpace( *e->vertex1 ), model->objectSpaceToWorldSpace( *e->vertex2 ), rgba( 255, 00, 00, 255 ),line_pattern(Int((Timer*10.0))Mod 16) )
	''		Else
	''			'drawPoint3d(cam,galaxy->objectSpaceToWorldSpace( *e->vertex1 ),10,col_s1)
	''			'		drawCircle3d(cam,galaxy->objectSpaceToWorldSpace( *e->vertex2 ),1,8,&h336666ff)
	''			'r=Temperature2RGB(Rnd*2000+2000+e->vertex2->distance(vec4(0,0,0))*100,127)
	''			'drawPoint3d(cam,model->objectSpaceToWorldSpace( *e->vertex2 ),10,r)
	''			drawLine3d( cam, model->objectSpaceToWorldSpace( *e->vertex1 ), model->objectSpaceToWorldSpace( *e->vertex2 ), rgba( 64, 64, 255, 255 ) ,line_pattern(Int((Timer*10.0))Mod 16))
	''		endif
	''	next
	''EndIf

	'dim as vec4 Ptr v,v1
	'Dim s As sector_t Ptr
	'Dim s1 As system_t Ptr
	''if( model->vertices->count > 0 ) Then
	''	Dim i As UInteger=Int(Rnd*model->vertices->count-1)
	''	'For i as integer = 0 to model->vertices->count-1
	''		v = model->vertices->get( i )
	''		s=sectors->Get(i)
	''		s1=s->systems->Get(0)
	''		If Rnd<(s1->star[0].luminousity/4000000)/(Abs(distance3d(v->x,v->y,v->z,0,0,0))) And (s->known=FALSE) Then
	''			known_systems->vertices->Add(New vec4(v->x,v->y,v->z))
	''			s->known=TRUE
	''			'model->vertices->remove(i)
	''						Draw String(10,350),"found",&hffffffff
	''		Else
	''						Draw String(10,350),"not found",&hffffffff

	''		EndIf
	''		'Print s
	''
	''		'If v->x=0 And v->y=0 And v->z=0 Then
	''		'	r=Temperature2RGB(s->kelvin,127)
	''		'	'r=get_rgb_peak_color(get_peak_frequency(s->kelvin),127)

	''		'	drawPoint3d(cam,model->objectSpaceToWorldSpace( *v ),s->size,r)
	''		'	'drawText3dshadowed(cam,model->objectSpaceToWorldSpace( *v ),s1->sys_name,r,&hff333333,8)
	''		'Else
	''		'	r=Temperature2RGB(s->kelvin,127)
	''		'	'r=get_rgb_peak_color(get_peak_frequency(s->kelvin),127)

	''		'	drawPoint3d(cam,model->objectSpaceToWorldSpace( *v ),s->size,r)
	''		'	'drawText3dshadowed(cam,model->objectSpaceToWorldSpace( *v ),s1->sys_name,r,&hff333333,8)
	''		'EndIf
	''	'next
	''end If

	'if( known_systems->vertices->count > 0 ) then
	'	for i as integer = 0 to known_systems->vertices->count-1
	'		v = known_systems->vertices->get( i )
	'		s=known_sectors->Get(i)
	'		s1=s->systems->Get(0)

	'		'Print s

	'		'dim as vec4 pos1

	'		'pos1 = cam.transform( *v - cam.getPos() )
	'		'		If clipPoint( cam, pos1) Then
	'		If v->x=0 And v->y=0 And v->z=0 Then
	'			r=Temperature2RGB(s->kelvin,a*127)
	'			'r=get_rgb_peak_color(get_peak_frequency(s->kelvin),127)

	'			drawPoint3d(cam,known_systems->objectSpaceToWorldSpace( *v ),s->size,r)
	'			'drawText3dshadowed(cam,known_systems->objectSpaceToWorldSpace( *v ),s1->sys_name,r,&hff333333,8)
	'		Else
	'			r=Temperature2RGB(s1->star[Int(Rnd*s1->nr_stars)].temperature,a*127)
	'			'r=Temperature2RGB(s->kelvin,127)
	'			'r=get_rgb_peak_color(get_peak_frequency(s->kelvin),127)

	'			drawPoint3d(cam,known_systems->objectSpaceToWorldSpace( *v ),s->size,r)
	'			'drawText3dshadowed(cam,known_systems->objectSpaceToWorldSpace( *v ),s1->sys_name,r,&hff333333,8)
	'		EndIf
	'		'Else
	'		'	Continue For
	'		'endif
	'	next
	'end If

End Sub

Sub galaxy_t.pre_render_model(cam As camera)
	'Dim r As uinteger
	'If model<>0 Then
	'	dim as vec4 Ptr v
	'	if( model->vertices->count > 0 ) then
	'		'Dim s As sector_t Ptr
	'		for i as integer = 0 to model->vertices->count-1
	'			's=sectors->Get(i)
	'			v = model->vertices->get( i )
	'			'				If MouseOver(cam,*v,200) Then Print "yes"
	'			If v->x=0 And v->y=0 And v->z=0 Then
	'				'drawPoint3d(cam,model->objectSpaceToWorldSpace( *v ),200,&hffff0000)
	'				drawPixel3d(cam,model->objectSpaceToWorldSpace( *v ),2,r)
	'			Else
	'				'r=Temperature2RGB(s->kelvin,127)
	'				r=Temperature2RGB(40000,127)
	'				'drawPoint3d(cam,model->objectSpaceToWorldSpace( *v ),200,r)
	'				drawPixel3d(cam,model->objectSpaceToWorldSpace( *v ),2,r)
	'			EndIf
	'		next
	'	end If
	'endif
End Sub
Sub galaxy_t.render_view_model(cam As camera,a As Single,s As boolean)
	'Dim r As UInteger
	'If s Then
	'	Line (cam.projectionPlane.x,cam.projectionPlane.y)-(cam.projectionPlane.x+cam.projectionPlane.width,cam.projectionPlane.y+cam.projectionPlane.height),&hff000000,BF
	'	Line (cam.projectionPlane.x,cam.projectionPlane.y)-(cam.projectionPlane.x+cam.projectionPlane.width,cam.projectionPlane.y+cam.projectionPlane.height),&hff00ff00,B
	'EndIf
	'If known_systems<>0 Then
	'	dim as vec4 Ptr v
	'	if( known_systems->vertices->count > 0 ) then
	'		Dim s As sector_t Ptr
	'		Dim sys As system_t Ptr
	'		for i as integer = 0 to known_systems->vertices->count-1
	'			s=known_sectors->Get(i)
	'			sys=s->systems->Get(0)
	'			v = known_systems->vertices->get( i )
	'			'				If MouseOver(cam,*v,200) Then Print "yes"
	'			If v->x=0 And v->y=0 And v->z=0 Then
	'				drawPixel3d(cam,known_systems->objectSpaceToWorldSpace( *v ),2,((a*255)Shl 24) Or &h00ff0000)
	'			Else
	'				r=Temperature2RGB(sys->star[Int(Rnd*sys->nr_stars)].temperature,a*255)
	'				'r=Temperature2RGB(40000,127)
	'				drawPixel3d(cam,known_systems->objectSpaceToWorldSpace( *v ),2,r)
	'			EndIf
	'			'Print sys->sys_name,sys->seed
	'		next
	'	end If
	'endif
End Sub
Sub galaxy_t.get_path(s As UInteger, t As uinteger)
	'Dim start As UInteger=s
	'Dim ende As UInteger=t
	'var pq = FbFrameWork.Collections.Queue( _
	'FbFramework.Collections.Queue.PriorityOrder.Ascending )

	'dim as integer numElements = model->vertices->count-1

	''' Add some elements to the queue
	'Dim v1 As vec4 Ptr=model->vertices->Get(s)
	'Dim v2 As vec4 Ptr
	'for i as integer = 1 to numElements
	'	v2=model->vertices->Get(i)
	'	dim as integer priority = distance3d(v1->x,v1->y,v1->z,v2->x,v2->y,v2->z)
	'	pq.push( priority, v2 )
	'next

	''' Retrieve them
	'for i as integer = 0 to pq.count - 1
	'	v2=pq.pop()
	'	path->vertices->Add(v2)
	'	Print v2->x,v2->y,v2->z
	'Next
	'Sleep
End Sub








'Dim Shared As object3d Ptr galaxy
'galaxy=New object3d()
'
'Sub generate_galaxy(nr_arms As UInteger, k As Single, p As Single)
'
'	Dim count As UInteger=0
'	Dim x As Double
'	Dim y As Double
'	Dim z As Double
'	'Dim nr_arms As UInteger=GetScrollBarVal(ScrollSpiralArms)
'	galaxy->vertices->Add( New vec4( 0,0,0 ))
'	For j As UInteger=0 To 360-360/nr_arms Step 360/nr_arms
'		'count+=1
'		For i As Single=0 To 1440 Step 1
'			'Dim As Single k=GetScrollBarVal(ScrollSpiralK)/10.0
'			'Dim As Single p=GetScrollBarVal(ScrollSpiralP)/10.0
'			Dim r1 As Single=(((i^2)/(k/p)^4))
'			If r1>=5.0 Then Exit For
'			y=0
'			x=0
'			z=0
'			For s As Single=0 To sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )*3 Step nr_arms
'				Dim p1 As vec4
'				x=(-0.5+Rnd)'*sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )
'				z=(-0.5+Rnd)'*sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )
'				y=(-0.5+Rnd)*sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )
'				p1.y=y*3.0
'				p1.x=Sin((i+j)*degToRad)*r1*10.0+x*3.0
'				p1.z=Cos((i+j)*degToRad)*r1*10.0+z*3.0
'
'				'galaxy_tmp->vertices->Add( New vec4( p1.x, p1.y, p1.z ) )
'				galaxy->vertices->Add( New vec4( p1.x, p1.y, p1.z ))
'				For s1 As single=0 To (sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 ))*1 Step 1
'					x=(-0.5+Rnd)'*sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )
'					z=(-0.5+Rnd)'*sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )
'					y=(-0.5+Rnd)*sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )
'					p1.y+=y*1.5
'					p1.x+=x*1.5
'					p1.z+=z*1.5
'					galaxy->vertices->Add( New vec4( p1.x, p1.y, p1.z ))
'					'					drawPoint3d(cam,vec4( p1.x, 0, p1.z ),sigmoid(map(r1,0,10,0,-8),GetScrollBarVal(ScrollBulgeSize)/100.0,GetScrollBarVal(ScrollBulgeOffset)/100.0 )*320.0,c)
'					count+=1
'				Next
'			Next
'		Next
'	Next
'
'	Color &hffffffff
'
'
'	Dim vert1_ptr As arrayList Ptr=galaxy->vertices()
'	Dim vert2_ptr As arrayList Ptr=galaxy->vertices()
'
'	Dim As UInteger from_counter_max=((galaxy->vertices->count-2))'*(galaxy_tmp->vertices->count)/2)-(galaxy_tmp->vertices->count-1)'(STARS*(STARS+1)/2) - STARS
'	Dim As UInteger from_counter'=from_counter_max
'	Dim As UInteger to_counter'=galaxy_tmp->vertices->count-1
'	Dim dist As Double
'	Dim vert1 As vec4 Ptr
'	Dim vert2 As vec4 Ptr
'	Dim As connection_t Ptr con1
'	Dim id1 As UInteger
'	Dim id2 As UInteger
'	Dim counter As uinteger=0
'
'	'generate distance array
'	Print "generate distance array"
'	For i As UInteger=0 To galaxy->vertices->count()-1
'		'Locate 1,1
'		'Print i/galaxy_tmp->vertices->count()*100
'		vert1 = vert1_ptr->Get(i)
'		For j As UInteger=i To galaxy->vertices->count()-1
'			vert2 = vert1_ptr->Get(j)
'			dist=distance3d(vert2->x,vert2->y,vert2->z,vert1->x,vert1->y,vert1->z)
'			If i<>j Then
'				con=New connection_t(dist,i,j)
'				connectionList.Add(con)
'			EndIf
'		Next
'	Next
'	'generate spannbaum
'	from_counter_max=connectionList.count()-1
'	sortList(connectionList)
'	While (to_counter<galaxy->vertices->count-1)
'		'	sortList(connectionList)
'		'sort distance array
'		Line(100,100)-(100+200,100+20),&hff000000,BF
'		Line(100,100)-(100+map(connectionList.count()/from_counter_max,1.0,0.0,0.0,200.0),100+20),&hff3333ff,BF
'		Line(100,100)-(100+200,100+20),&hffffffff,B
'		Draw String(120,108),str(connectionList.count()/from_counter_max*100),&hffffffff
'		con1=connectionList.get(0)
'		galaxy->edges->add( new edge( vert1_ptr->Get(con1->node_1),vert1_ptr->Get(con1->node_2) ) )
'		counter=0
'		id1 =con1->node_1
'		id2 =con1->node_2
'		dist=con1->dist
'		While (counter<connectionList.count()-1)
'			con1=connectionList.get(counter)
'			If (con1->node_2=id2) Then 'And (con1->node_2<>id1)Then'Or (con1->node_2=id2)Then'and not(con1->node_1=id2) Then
'				connectionList.remove(counter)
'				counter-=1
'			EndIf
'			counter+=1
'		Wend
'		to_counter+=1
'	Wend
'End Sub

'Sub quicksort(list As arrayList, l As Long, r As Long)
'	Dim As connection_t Ptr con1,con2
'
'	Dim As ULong size = r - l +1
'	If size < 2 Then Exit Sub
'	con1=list.get(l + size \ 2)
'
'	Dim As Long i = l, j = r
'	Dim As double pivot = con1->dist
'
'	Do
'		con1=list.get(i)
'		While con1->dist < pivot
'			i += 1
'			con1=list.get(i)
'		Wend
'		con1=list.get(j)
'		While pivot < con1->dist
'			j -= 1
'			con1=list.get(j)
'		Wend
'		If i <= j Then
'			list.swapa(i,j)
'			i += 1
'			j -= 1
'		End If
'	Loop Until i > j
'
'	If l < j Then quicksort(list, l, j)
'	If i < r Then quicksort(list, i, r)
'
'End Sub
'Sub sortList(list As arrayList)
'	'Print "sort distance array...",connectionList.count()-1
'	quicksort(list,0,list.count-1)
'	'	Print "sorting done."
'End Sub





'Sub generate_sectors(n As vec4 Ptr,epoch As Double,nr_sys As uinteger)
'
'	Dim nr_systems As UInteger=nr_sys'n->connections.count
'	'Print nr_sys
'	'If nr_systems>0 Then
'	'While nr_systems>0
'	Dim s As sector_t Ptr=New sector_t(n)
'	s->generate_systems(nr_systems)
'	s->size=80*nr_systems
'	For i As UInteger=0 To nr_systems-1
'		Dim sys As system_t ptr=s->systems->Get(i)
'		For j As UInteger=0 To sys->nr_stars-1
'			s->kelvin+=sys->star[j].temperature
'		Next
'		s->kelvin/=sys->nr_stars
'	Next
'	s->epoch=epoch
'	sectors->Add(s)
'	'nr_systems-=1
'	'Wend
'	'EndIf
'End Sub
'Sub fade_in(sec As sector_t,sys As system_t)
'
'End Sub
'
'
'
'Function Regulate(Byval MyFps As double,Byref fpss As double) As double
'    Static As Double timervalue,lastsleeptime,t3,frames
'    Dim As Double t=Timer
'    frames+=1
'    If (t-t3)>=1 Then t3=t:fpss=frames:frames=0
'    Dim As Double sleeptime=lastsleeptime+((1/myfps)-T+timervalue)*1000
'    If sleeptime<1 Then sleeptime=1
'    lastsleeptime=sleeptime
'    timervalue=T
'    Return sleeptime
'End Function
