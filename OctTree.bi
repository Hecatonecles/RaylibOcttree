#Include Once "./inc/raylib/raylib.bi"
#Include Once "./inc/raylib/raymath.bi"
#Include Once "arraylist.bi"
#Include Once "crt/limits.bi"
Type Cuboid
	Declare Constructor()
	Declare Constructor(p As Vector3,s As Vector3)
	Declare Destructor()
	posi As Vector3
	size As Vector3
	mmin As Vector3
	mmax As Vector3
	'BBox(0 To 7) As Vector3
	Declare Function PointIsOnRay(p As Vector3,o As vector3,d As vector3) As boolean
	Declare Function PointIsOnPlane(p As Vector3,v1 As vector3,v2 As vector3,v3 As vector3,v4 As vector3) As boolean
	Declare Function PointIsInsideBBox(p As Vector3) As boolean
	Declare Function PointIsInsideBSphere(p As Vector3,sp As Vector3,r As single) As boolean
	Declare Function CubeOverlapsBBox(b As Cuboid) As boolean
	Declare Function SphereOverlapsBBox(p As vector3,r As single) As boolean
	Declare Function PlaneOverlapsBBox(v1 As vector3,v2 As vector3,v3 As vector3,v4 As vector3) As boolean
	Declare Function RayOverlapsBBox(o As vector3,d As vector3) As boolean
End Type
Constructor Cuboid()

End Constructor
Destructor Cuboid()

End Destructor
Constructor Cuboid(p As Vector3,s As Vector3)
posi=p
size=s
mmin.x=posi.x-size.x/2
mmax.x=posi.x+size.x/2
mmin.y=posi.y-size.y/2
mmax.y=posi.y+size.y/2
mmin.z=posi.z-size.z/2
mmax.z=posi.z+size.z/2
If mmin.x>mmax.x Then Swap mmin.x,mmax.x
If mmin.y>mmax.y Then Swap mmin.y,mmax.y
If mmin.z>mmax.z Then Swap mmin.z,mmax.z
End Constructor
Function Cuboid.PointIsInsideBBox(p As Vector3) As boolean
If ((p.x>=mmin.x) And (p.x<mmax.x) And (p.y>=mmin.y) And (p.y<mmax.y) And (p.z>=mmin.z) And (p.z<mmax.z)) Then Return TRUE
Return FALSE
End Function
Function Cuboid.CubeOverlapsBBox(b As Cuboid) As boolean
	If ((b.mmin.x<=mmax.x) and (b.mmax.x>=mmin.x) and (b.mmin.y<=mmax.y) and (b.mmax.y>=mmin.y) and (b.mmin.z<=mmax.z) And (b.mmax.z>=mmin.z)) Then Return TRUE
	If ((mmin.x<=b.mmax.x) and (mmax.x>=b.mmin.x) and (mmin.y<=b.mmax.y) and (mmax.y>=b.mmin.y) and (mmin.z<=b.mmax.z) And (mmax.z>=b.mmin.z)) Then Return TRUE
	Return FALSE
End Function
Function Cuboid.PointIsInsideBSphere(p As Vector3,sp As Vector3,r As Single) As boolean
	Dim x1 As Single=(p.x-sp.x)*(p.x-sp.x)
	Dim y1 As Single=(p.y-sp.y)*(p.y-sp.y)
	Dim z1 As Single=(p.z-sp.z)*(p.z-sp.z)
	Dim ans As Single=x1+y1+z1
	If ans<=(r*r) Then Return true
'If ((p.x>=sp.x-r/2) And (p.x<=sp.x+r/2) And (p.y>=sp.y-r/2) And (p.y<=sp.y+r/2) And (p.z>=sp.z-r/2) And (p.z<=sp.z+r/2)) Then Return TRUE
Return FALSE
End Function
Function Cuboid.SphereOverlapsBBox(p As vector3,r As Single) As boolean
	 ' get box closest point to sphere center by clamping
	 Dim max As Single=IIf(mmin.x>p.x,mmin.x,p.x)
	 Dim may As Single=IIf(mmin.y>p.y,mmin.y,p.y)
	 Dim maz As Single=IIf(mmin.z>p.z,mmin.z,p.z)
  dim x As Single= iif(max<mmax.x,max,mmax.x)
  dim y As Single= iif(may<mmax.y,may,mmax.y)
  dim z As Single= iif(maz<mmax.z,maz,mmax.z)
  ' this is the same as isPointInsideSphere
  Dim As single distance = ((x - p.x) * (x - p.x) +(y - p.y) * (y - p.y) + (z - p.z) * (z - p.z))
  return (distance <= (r*r))
End Function
Function Cuboid.RayOverlapsBBox(o As vector3,d As vector3) As boolean
	'bool intersection(box b, ray r) {
    Dim As Double tmin = DBL_MIN
    Dim As Double tmax = DBL_MAX
	 Dim As Double tx1,tx2,ty1,ty2,tz1,tz2
    if (d.x <> 0.0)  Then
        tx1 = (mmin.x - o.x)/d.x
        tx2 = (mmax.x - o.x)/d.x

        tmin = max(tmin, min(tx1, tx2))
        tmax = min(tmax, max(tx1, tx2))
    EndIf

    if (d.y <> 0.0) Then
        ty1 = (mmin.y - o.y)/d.y
        ty2 = (mmax.y - o.y)/d.y

        tmin = max(tmin, min(ty1, ty2))
        tmax = min(tmax, max(ty1, ty2))
    EndIf
    if (d.z <> 0.0) Then
        tz1 = (mmin.z - o.z)/d.z
        tz2 = (mmax.z - o.z)/d.z

        tmin = max(tmin, min(tz1, tz2))
        tmax = min(tmax, max(tz1, tz2))
    EndIf

    return tmax >= tmin
End Function

Function Cuboid.PointIsOnPlane(p As Vector3,v1 As vector3,v2 As vector3,v3 As vector3,v4 As vector3) As boolean
 Return true
'If ((p.x>=sp.x-r/2) And (p.x<=sp.x+r/2) And (p.y>=sp.y-r/2) And (p.y<=sp.y+r/2) And (p.z>=sp.z-r/2) And (p.z<=sp.z+r/2)) Then Return TRUE
Return FALSE
End Function
Function Cuboid.PointIsOnRay(p As Vector3,o As vector3,d As vector3) As boolean
	Dim s As vector3=Vector3Subtract(p,o)
	s=vector3Normalize(s)
	d=vector3Normalize(d)
	Dim a As vector3=Vector3Subtract(d,s)
	'Print a.x,a.y,a.z
	If (a.x<=0.0001) And (a.y<=0.0001) And (a.z<=0.0001) And _
	(a.x>=-0.0001) And (a.y>=-0.0001) And (a.z>=-0.0001) Then Return TRUE
	Return FALSE
End Function



Type OctTree
	Declare Constructor()
	Declare Constructor(bounds As Cuboid,cap As Long)
	Declare Destructor()
	Declare Sub insert(p As Vector3 Ptr)
	Declare sub getPointsInCube(bound As Cuboid,arr As arrayList ptr)
	Declare sub getPointsInSphere(sp As vector3,r As Single,arr As arrayList ptr)
	Declare sub getPointsOnPlane(v1 As vector3,v2 As vector3,v3 As vector3,v4 As vector3,arr As arrayList ptr)
	Declare sub getPointsOnRay(origin As vector3,direction As vector3,arr As arrayList ptr)
	Declare Sub render()

	bounds As Cuboid
	points As arrayList Ptr'Vector3

	capacity As Long
	subdivided As boolean
	Childs As OctTree ptr
End Type
Constructor OctTree(bounds As Cuboid,cap As Long)
this.bounds=bounds
this.capacity=cap
this.subdivided=FALSE
this.Childs=NULL
this.points= New arrayList
'Print "Construct"
End Constructor
Constructor OctTree()

End Constructor
Destructor OctTree()

End Destructor

Sub OctTree.insert(p As Vector3 Ptr)
	If Not(this.bounds.PointIsInsideBBox(*p)) Then Exit Sub
	If (this.points->count<this.capacity) then
		this.points->add(p)
		'Print "insert"
		'this.subdivided=FALSE
	ElseIf this.subdivided=FALSE then
		'Print "subdivide"
		this.Childs=New OctTree[8]'RL_CALLOC(8,SizeOf(OctTree))
		Dim b As Cuboid
		Dim sp As Vector3
		Dim sv As Vector3=Vector3Scale(this.bounds.size,0.5)
		sp=Vector3(bounds.posi.x-sv.x/2,bounds.posi.y-sv.y/2,bounds.posi.z-sv.z/2)
		'im blind of a solution. how to calculate the boundary of the cuboid as fast as posible and in the right way?
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		Childs[0].Constructor(b,this.capacity)
		'Childs[0].bounds=b
		'Childs[0].capacity=this.Capacity
		'Childs[0].subdivided=FALSE

		sp=Vector3(bounds.posi.x+sv.x/2,bounds.posi.y-sv.y/2,bounds.posi.z-sv.z/2)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		'im blind of a solution. how to calculate the boundary of the cuboid as fast as posible and in the right way?
		Childs[1].Constructor(b,this.capacity)
		'Childs[1].bounds=b'Vector3Add(b.posi,Vector3Scale(b.size,0.5)) 'this seams to me wrong
		'Childs[1].capacity=this.Capacity
		'Childs[1].subdivided=FALSE

		sp=Vector3(bounds.posi.x-sv.x/2,bounds.posi.y+sv.y/2,bounds.posi.z-sv.z/2)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		'im blind of a solution. how to calculate the boundary of the cuboid as fast as posible and in the right way?
		Childs[2].Constructor(b,this.capacity)
		'Childs[2].bounds=b'Vector3Add(b.posi,Vector3Scale(b.size,0.5)) 'this seams to me wrong
		'Childs[2].capacity=this.Capacity
		'Childs[2].subdivided=FALSE

		sp=Vector3(bounds.posi.x+sv.x/2,bounds.posi.y+sv.y/2,bounds.posi.z-sv.z/2)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		'im blind of a solution. how to calculate the boundary of the cuboid as fast as posible and in the right way?
		Childs[3].Constructor(b,this.capacity)
		'Childs[3].bounds=b'Vector3Add(b.posi,Vector3Scale(b.size,0.5)) 'this seams to me wrong
		'Childs[3].capacity=this.Capacity
		'Childs[3].subdivided=FALSE

		sp=Vector3(bounds.posi.x-sv.x/2,bounds.posi.y-sv.y/2,bounds.posi.z+sv.z/2)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		'im blind of a solution. how to calculate the boundary of the cuboid as fast as posible and in the right way?
		Childs[4].Constructor(b,this.capacity)
		'Childs[4].bounds=b'Vector3Add(b.posi,Vector3Scale(b.size,0.5)) 'this seams to me wrong
		'Childs[4].capacity=this.Capacity
		'Childs[4].subdivided=FALSE

		sp=Vector3(bounds.posi.x-sv.x/2,bounds.posi.y+sv.y/2,bounds.posi.z+sv.z/2)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		'im blind of a solution. how to calculate the boundary of the cuboid as fast as posible and in the right way?
		Childs[5].Constructor(b,this.capacity)
		'Childs[5].bounds=b'Vector3Add(b.posi,Vector3Scale(b.size,0.5)) 'this seams to me wrong
		'Childs[5].capacity=this.Capacity
		'Childs[5].subdivided=FALSE

		sp=Vector3(bounds.posi.x+sv.x/2,bounds.posi.y-sv.y/2,bounds.posi.z+sv.z/2)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		'im blind of a solution. how to calculate the boundary of the cuboid as fast as posible and in the right way?
		Childs[6].Constructor(b,this.capacity)
		'Childs[6].bounds=b'Vector3Add(b.posi,Vector3Scale(b.size,0.5)) 'this seams to me wrong
		'Childs[6].capacity=this.Capacity
		'Childs[6].subdivided=FALSE

		sp=Vector3(bounds.posi.x+sv.x/2,bounds.posi.y+sv.y/2,bounds.posi.z+sv.z/2)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		'im blind of a solution. how to calculate the boundary of the cuboid as fast as posible and in the right way?
		Childs[7].Constructor(b,this.capacity)
		'Childs[7].bounds=b'Vector3Add(b.posi,Vector3Scale(b.size,0.5)) 'this seams to me wrong
		'Childs[7].capacity=this.Capacity
		'Childs[7].subdivided=FALSE

		this.subdivided=TRUE
	EndIf
	If Childs<>NULL Then
		Childs[0].insert(p)
		Childs[1].insert(p)
		Childs[2].insert(p)
		Childs[3].insert(p)
		Childs[4].insert(p)
		Childs[5].insert(p)
		Childs[6].insert(p)
		Childs[7].insert(p)
	EndIf
	'endif
End Sub
Sub OctTree.getPointsInCube(bound As Cuboid,arr As arrayList Ptr)
	If (this.bounds.CubeOverlapsBBox(bound)) Then
		'Else
		For i As Long =0 To this.points->count-1
			Dim p As Vector3 Ptr=points->Get(i)
			If bound.PointIsInsideBBox(*p) Then
				arr->add(p)
			EndIf
		Next
		If Childs<>NULL Then
			For i As Long = 0 To 7
				Childs[i].getPointsInCube(bound,arr)
			Next
		EndIf
	EndIf

End Sub
Sub OctTree.getPointsInSphere(sp As vector3,r As single,arr As arrayList Ptr)
	If (this.bounds.SphereOverlapsBBox(sp,r)) Then
		'Else
		For i As Long =0 To this.points->count-1
			Dim p As Vector3 Ptr=points->Get(i)
			If This.bounds.PointIsInsideBSphere(*p,sp,r) Then
				arr->add(p)
			EndIf
		Next
		If Childs<>NULL Then
			For i As Long = 0 To 7
				Childs[i].getPointsInSphere(sp,r,arr)
			Next
		EndIf
	EndIf

End Sub
sub OctTree.getPointsOnRay(origin As vector3,direction As vector3,arr As arrayList ptr)
		If (this.bounds.RayOverlapsBBox(origin,direction)) Then
		'Else
		For i As Long =0 To this.points->count-1
			Dim p As Vector3 Ptr=points->Get(i)
			If This.bounds.PointIsOnRay(*p,origin,direction) Then
				arr->add(p)
			EndIf
		Next
		If Childs<>NULL Then
			For i As Long = 0 To 7
				Childs[i].getPointsOnRay(origin,direction,arr)
			Next
		EndIf
	EndIf
End Sub

Sub OctTree.render()
	'Print this.subdivided,this.points->count
	If (this.subdivided=TRUE) Then
		'Print "Child"
		If (this.Childs<>NULL) Then
			For i As Long=0 To 7
				this.Childs[i].render()
			Next
		EndIf
	Else
		DrawCubeWiresV(this.bounds.posi,this.bounds.size,RayColor(255,0,0,255))
	EndIf
	For i As Long=0 To this.points->count-1
		Dim p As Vector3 Ptr=this.points->get(i)
		DrawCube(*p,.5,.5,.5,RayColor(255,0,0,255))
	Next

End Sub
