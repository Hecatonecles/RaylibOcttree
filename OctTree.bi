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
	Declare Function PointIsOnRay(p As Vector3,o As vector3,d As vector3,r As Single=1) As boolean
	Declare Function PointIsOnPlane(p As Vector3,n As vector3,d As Single=1) As single
	Declare Function PointIsOnFrontOffPlane(p As Vector3,v0 As vector3,n As vector3,d As Single=1) As boolean
	Declare Function PointIsOnBackOffPlane(p As Vector3,v0 As vector3,n As vector3,d As Single=1) As boolean
	Declare Function PointIsOnCube(p As Vector3,t As Single=1) As boolean
	Declare Function PointIsInsideCube(p As Vector3) As boolean
	Declare Function PointIsOutsideCube(p As Vector3) As boolean
	Declare Function PointIsInsideSphere(p As Vector3,sp As Vector3,r As single) As boolean
	Declare Function PointIsOnSphere(p As Vector3,sp As Vector3,r As Single,t As Single=1) As boolean
	Declare Function PointIsOutsideSphere(p As Vector3,sp As Vector3,r As single) As boolean
	Declare Function PointIsInsideCylinder(p As vector3,co As Vector3, cd As Vector3, length As single, radius As single) As single
	Declare Function CubeOverlapsCube(b As Cuboid) As boolean
	Declare Function SphereOverlapsCube(p As vector3,r As single) As boolean
	Declare Function CylinderOverlapsCube(co As Vector3, cd As Vector3, length As single, radius As Single) As boolean
	Declare Function PlaneOverlapsCube(n As vector3,d As Single=1) As boolean
	Declare Function RayOverlapsCube(o As vector3,d As vector3,r As Single=0.00001) As boolean
End Type
Constructor Cuboid()

End Constructor
Destructor Cuboid()

End Destructor
Constructor Cuboid(p As Vector3,s As Vector3)
posi=p
size=s
mmin.x=posi.x-size.x*0.5
mmax.x=posi.x+size.x*0.5
mmin.y=posi.y-size.y*0.5
mmax.y=posi.y+size.y*0.5
mmin.z=posi.z-size.z*0.5
mmax.z=posi.z+size.z*0.5
If mmin.x>mmax.x Then Swap mmin.x,mmax.x
If mmin.y>mmax.y Then Swap mmin.y,mmax.y
If mmin.z>mmax.z Then Swap mmin.z,mmax.z
End Constructor
Function Cuboid.PointIsInsideCube(p As Vector3) As boolean
	If ((p.x>=mmin.x) And (p.x<mmax.x) And (p.y>=mmin.y) And (p.y<mmax.y) And (p.z>=mmin.z) And (p.z<mmax.z)) Then Return TRUE
	Return FALSE
End Function
Function Cuboid.PointIsOnCube(p As Vector3,t As Single=1) As boolean
	If ((p.x<mmin.x+t) And (p.x>mmax.x-t) And (p.y<mmin.y+t) And (p.y>mmax.y-t) And (p.z<mmin.z+t) And (p.z>mmax.z-t)) Then Return TRUE
	Return FALSE
End Function
Function Cuboid.CubeOverlapsCube(b As Cuboid) As boolean
	If ((b.mmin.x<=mmax.x) and (b.mmax.x>=mmin.x) and (b.mmin.y<=mmax.y) and (b.mmax.y>=mmin.y) and (b.mmin.z<=mmax.z) And (b.mmax.z>=mmin.z)) Then Return TRUE
	If ((mmin.x<=b.mmax.x) and (mmax.x>=b.mmin.x) and (mmin.y<=b.mmax.y) and (mmax.y>=b.mmin.y) and (mmin.z<=b.mmax.z) And (mmax.z>=b.mmin.z)) Then Return TRUE
	Return FALSE
End Function
Function Cuboid.PointIsInsideSphere(p As Vector3,sp As Vector3,r As Single) As Boolean
	Dim x1 As Single=(p.x-sp.x)*(p.x-sp.x)
	Dim y1 As Single=(p.y-sp.y)*(p.y-sp.y)
	Dim z1 As Single=(p.z-sp.z)*(p.z-sp.z)
	Dim ans As Single=x1+y1+z1
	If ans<=(r*r) Then Return true
	'If ((p.x>=sp.x-r/2) And (p.x<=sp.x+r/2) And (p.y>=sp.y-r/2) And (p.y<=sp.y+r/2) And (p.z>=sp.z-r/2) And (p.z<=sp.z+r/2)) Then Return TRUE
	Return FALSE
End Function
Function Cuboid.PointIsOnSphere(p As Vector3,sp As Vector3,r As Single,t As Single=1) As Boolean
	Dim x1 As Single=(p.x-sp.x)*(p.x-sp.x)
	Dim y1 As Single=(p.y-sp.y)*(p.y-sp.y)
	Dim z1 As Single=(p.z-sp.z)*(p.z-sp.z)
	Dim ans As Single=x1+y1+z1
	If (ans-t<=((r*r))) And (ans+t>=((r*r))) Then Return true
	'If ((p.x>=sp.x-r/2) And (p.x<=sp.x+r/2) And (p.y>=sp.y-r/2) And (p.y<=sp.y+r/2) And (p.z>=sp.z-r/2) And (p.z<=sp.z+r/2)) Then Return TRUE
	Return FALSE
End Function
Function Cuboid.PointIsOutsideSphere(p As Vector3,sp As Vector3,r As Single) As Boolean
	Dim x1 As Single=(p.x-sp.x)*(p.x-sp.x)
	Dim y1 As Single=(p.y-sp.y)*(p.y-sp.y)
	Dim z1 As Single=(p.z-sp.z)*(p.z-sp.z)
	Dim ans As Single=x1+y1+z1
	If ans>(r*r)Then Return true
	'If ((p.x>=sp.x-r/2) And (p.x<=sp.x+r/2) And (p.y>=sp.y-r/2) And (p.y<=sp.y+r/2) And (p.z>=sp.z-r/2) And (p.z<=sp.z+r/2)) Then Return TRUE
	Return FALSE
End Function
Function Cuboid.SphereOverlapsCube(p As vector3,r As Single) As boolean
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
Function Cuboid.PlaneOverlapsCube(n As vector3,d As Single=1) As boolean
	' Test if AABB b intersects plane p
	' Convert AABB to center-extents representation
	Dim As Vector3 c = Vector3Scale(Vector3Add(this.mmax, this.mmin), 0.5f) ' Compute AABB center
	Dim As Vector3 e = Vector3Subtract(this.mmax, c) ' Compute positive extents
	'Dim d As Single=-(n.x*v0.x)-(n.y*v0.y)-(n.z*v0.z)
	' Compute the projection interval radius of b onto L(t) = b.c + t * p.n
	Dim As Single r = e.x*Abs(n.x) + e.y*Abs(n.y) + e.z*Abs(n.z)

	' Compute distance of box center from plane
	Dim As single s = Vector3DotProduct(n, c) - d

	' Intersection occurs when distance s falls within [-r,+r] interval
	return Abs(s) <= r
End Function
Function Cuboid.RayOverlapsCube(o As vector3,d As vector3,r As Single=0.00001) As boolean
	'bool intersection(box b, ray r) {
	Dim As Double tmin = DBL_MIN
	Dim As Double tmax = DBL_MAX
	Dim As Double tx1,tx2,ty1,ty2,tz1,tz2
	if (d.x <> 0.0)  Then
		tx1 = (mmin.x - (o.x))/d.x
		tx2 = (mmax.x - (o.x))/d.x

		tmin = max(tmin, min(tx1, tx2))
		tmax = min(tmax, max(tx1, tx2))
	EndIf

	if (d.y <> 0.0) Then
		ty1 = (mmin.y - (o.y))/d.y
		ty2 = (mmax.y - (o.y))/d.y

		tmin = max(tmin, min(ty1, ty2))
		tmax = min(tmax, max(ty1, ty2))
	EndIf
	if (d.z <> 0.0) Then
		tz1 = (mmin.z - (o.z))/d.z
		tz2 = (mmax.z - (o.z))/d.z

		tmin = max(tmin, min(tz1, tz2))
		tmax = min(tmax, max(tz1, tz2))
	EndIf

	return ((tmax*r) >= (tmin*r))
End Function

Function Cuboid.PointIsOnPlane(p As Vector3,n As vector3,d As Single=1) As Single
	'signed distance of point P to plane V0,N
	dim as single    sb, sn, sd
	Dim v0 As Vector3
	v0=Vector3Add(Vector3(0,0,0),Vector3Scale(n,d))
	sn = -Vector3DotProduct( n, Vector3Subtract(p, v0))
	sd = Vector3DotProduct(n, n)
	sb = sn / sd
	Return sb
	'Dim as Vector3 B = Vector3Add(p ,Vector3Scale( n , sb))
	'B=(Vector3Subtract(p, B))
	'Print B.x,B.y,B.z
	'Dim l As Single=Vector3Length(B)
	'Print b.x,b.y,b.z,l
	'If l<=1 And l>=-0 Then Return true
	'If (B.x<=t) And (B.x>=-t) And (B.y<=t) And (B.y>=-t) And (B.z<=t) And (B.z>=-t) Then Return TRUE
	
	Return FALSE
End Function
Function Cuboid.PointIsOnRay(p As Vector3,o As vector3,d As vector3,r As Single=1) As boolean
	Dim gpo As vector3=Vector3Subtract(p,o)
	gpo=vector3Normalize(gpo)
	d=vector3Normalize(d)
	Dim c As vector3=Vector3CrossProduct(o,d)
	Dim dgpo_diff As vector3=Vector3Subtract(d,gpo)
	dgpo_diff=vector3Normalize(dgpo_diff)
	'Dim rr As Vector3=Vector3Scale(c,r)
	'r=Vector3Length(rr)
	'Print a.x,a.y,a.z
	If (dgpo_diff.x<=r) And (dgpo_diff.y<=r) And (dgpo_diff.z<=r) And _
	(dgpo_diff.x>=-r) And (dgpo_diff.y>=-r) And (dgpo_diff.z>=-r) Then Return TRUE
	Return FALSE
End Function
Function Cuboid.PointIsInsideCylinder(p As vector3,co As Vector3, cd As Vector3, length As single, radius As single) As Single

	Dim As Single dx, dy, dz	' vector d  from line segment point 1 to point 2
	Dim As Single pdx, pdy, pdz ' vector pd from point 1 to test point
	Dim As Single dot, dsq

	dx = (cd.x*length) - co.x	' translate so pt1 is origin.  Make vector from
	dy = (cd.y*length) - co.y  ' pt1 to pt2.  Need for this is easily eliminated
	dz = (cd.z*length) - co.z

	pdx = p.x - co.x		' vector from pt1 to test point.
	pdy = p.y - co.y
	pdz = p.z - co.z

	' Dot the d and pd vectors to see if point lies behind the
	' cylinder cap at pt1.x, pt1.y, pt1.z

	dot = (pdx * dx + pdy * dy + pdz * dz)

	' If dot is less than zero the point is behind the pt1 cap.
	' If greater than the cylinder axis line segment length squared
	' then the point is outside the other end cap at pt2.
	Dim lengthsq As Single=length*length
	if( ((dot) > 0.0f) and ((dot) < lengthsq) ) Then
		'	Return FALSE
		'else
		' Point lies within the parallel caps, so find
		' distance squared from point to line, using the fact that sin^2 + cos^2 = 1
		' the dot = cos() * |d||pd|, and cross*cross = sin^2 * |d|^2 * |pd|^2
		' Carefull: '*' means mult for scalars and dotproduct for vectors
		' In short, where dist is pt distance to cyl axis:
		' dist = sin( pd to d ) * |pd|
		' distsq = dsq = (1 - cos^2( pd to d)) * |pd|^2
		' dsq = ( 1 - (pd * d)^2 / (|pd|^2 * |d|^2) ) * |pd|^2
		' dsq = pd * pd - dot * dot / lengthsq
		'  where lengthsq is d*d or |d|^2 that is passed into this function

		' distance squared to the cylinder axis:

		dsq = (pdx*pdx + pdy*pdy + pdz*pdz) - (dot*dot)/lengthsq
		Dim radiussq As Single=radius*radius
		if( dsq > radiussq ) Then
			Return -1
		Else
			Return dsq  ' return distance squared to axis
		EndIf
	EndIf
End Function
Function Cuboid.CylinderOverlapsCube(co As Vector3, cd As Vector3, length As single, radius As Single) As boolean
	Return TRUE
End Function

Type OctTree
	Declare Constructor(bounds As Cuboid,cap As Long)
	Declare Destructor()
	Declare Sub insert(p As Vector3 Ptr)
	Declare sub getPointsInCube(arr As arrayList Ptr,bound As Cuboid)
	Declare Sub getPointsOnCube(arr As arrayList Ptr,bound As Cuboid,t As Single=1)
	Declare sub getPointsOutCube(arr As arrayList Ptr,bound As Cuboid)
	Declare sub getPointsInSphere(arr As arrayList Ptr,sp As vector3,r As Single)
	Declare sub getPointsOutSphere(arr As arrayList Ptr,sp As vector3,r As Single)
	Declare sub getPointsOnSphere(arr As arrayList Ptr,sp As vector3,r As Single,t As Single=.1)
	Declare sub getPointsInCylinder(arr As arrayList Ptr,co As vector3,cd As Vector3,length As Single,r As Single)
	Declare sub getPointsOnPlane(arr As arrayList Ptr,n As vector3,d As Single=1)
	Declare sub getPointsOnRay(arr As arrayList Ptr,origin As vector3,direction As vector3,r As Single=0.00001)

	Declare sub getCuboidsInCube(arr As arrayList Ptr,bound As Cuboid)
	Declare sub getCuboidsInSphere(arr As arrayList Ptr,sp As vector3,r As Single)
	Declare sub getCuboidsOnPlane(arr As arrayList Ptr,v0 As vector3,n As vector3)
	Declare sub getCuboidsOnRay(arr As arrayList Ptr,v0 As vector3,n As vector3)
	Declare Sub render()
	Declare Sub clean()

	bounds As Cuboid
	points As arrayList'Vector3

	capacity As Long
	scapacity As Long
	subdivided As boolean
	As OctTree Ptr Childs (0 To 7)
End Type
Constructor OctTree(bounds As Cuboid,cap As Long)
this.bounds=bounds
this.capacity=cap
this.scapacity=cap
this.subdivided=FALSE
For i As Long=0 To 7
	If Childs(i)<>NULL Then
		Delete this.Childs(i)
		this.Childs(i)=NULL
		'Print"Childs Deleted"
	EndIf
Next
'Print "Construct"
End Constructor

Destructor OctTree()
If this.subdivided=FALSE Then
	If this.points.count>0 Then
		while (this.points.count>0)
			Dim p As Vector3 Ptr=points.removeLast()
			Delete p
			'p=NULL
			'Print "Point Deleted ";i
		Wend
	EndIf
Else
	For i As Long=0 To 7
		If Childs(i)<>NULL Then
			'Print "Cleaning Child ";i
			'this.Childs(i)->clean()
			'this.Childs[i].subdivided=false
			'	Delete (Childs[i])
			'	'This.Childs[i]=NULL
			Delete this.Childs(i)
			this.Childs(i)=NULL
			'Print"Childs Deleted"
			'				Print "here"',i

		EndIf
	Next
EndIf
this.subdivided=FALSE
this.capacity=this.scapacity
Dim b As Cuboid=Cuboid(Vector3(0,0,0),Vector3(256,256,256))
this.bounds=b

End Destructor
Sub OctTree.clean()
	If this.subdivided=FALSE Then
		If this.points.count>0 Then
			while (this.points.count>0)
				Dim p As Vector3 Ptr=points.removeLast()
				Delete p
				p=NULL
				'Print "Point Deleted ";i
			Wend
		EndIf
	Else

		For i As Long=0 To 7
			If Childs(i)<>NULL Then
				'Print "Cleaning Child ";i
				this.Childs(i)->clean()
				'this.Childs[i].subdivided=false
				'	Delete (Childs[i])
				'	'This.Childs[i]=NULL
				Delete this.Childs(i)
				this.Childs(i)=NULL
				'Print"Childs Deleted"
				'				Print "here"',i

			EndIf
		Next
	EndIf
	this.subdivided=FALSE
	this.capacity=this.scapacity
	'this.subdivided=FALSE
	'this.capacity=64'this.scapacity
	'Dim b As Cuboid=Cuboid(Vector3(0,0,0),Vector3(256,256,256))
	'this.bounds=b

End Sub
Sub OctTree.insert(p As Vector3 Ptr)
	If Not(this.bounds.PointIsInsideCube(*p)) Then Exit Sub

	If (this.capacity>=0) Then'And Not(this.subdivided) Then
		this.points.Add(p)
		this.capacity-=1
		'EndIf
	ElseIf not(this.subdivided) And (this.capacity<0) Then
		'this.Childs=New OctTree[8]'RL_CALLOC(8,SizeOf(OctTree))
		Dim b As Cuboid
		Dim sp As Vector3
		Dim sv As Vector3=Vector3Scale(this.bounds.size,0.5)
		Dim sv2 As Vector3=Vector3Scale(sv,0.5)
		sp=Vector3(bounds.posi.x-sv2.x,bounds.posi.y-sv2.y,bounds.posi.z-sv2.z)

		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		Childs(0)=New OctTree(b,this.scapacity)


		sp=Vector3(bounds.posi.x+sv2.x,bounds.posi.y-sv2.y,bounds.posi.z-sv2.z)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		Childs(1)=New OctTree(b,this.scapacity)

		sp=Vector3(bounds.posi.x-sv2.x,bounds.posi.y+sv2.y,bounds.posi.z-sv2.z)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		Childs(2)=New OctTree(b,this.scapacity)

		sp=Vector3(bounds.posi.x+sv2.x,bounds.posi.y+sv2.y,bounds.posi.z-sv2.z)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		Childs(3)=New OctTree(b,this.scapacity)

		sp=Vector3(bounds.posi.x-sv2.x,bounds.posi.y-sv2.y,bounds.posi.z+sv2.z)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		Childs(4)=New OctTree(b,this.scapacity)

		sp=Vector3(bounds.posi.x-sv2.x,bounds.posi.y+sv2.y,bounds.posi.z+sv2.z)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		Childs(5)=New OctTree(b,this.scapacity)

		sp=Vector3(bounds.posi.x+sv2.x,bounds.posi.y-sv2.y,bounds.posi.z+sv2.z)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		Childs(6)=New OctTree(b,this.scapacity)

		sp=Vector3(bounds.posi.x+sv2.x,bounds.posi.y+sv2.y,bounds.posi.z+sv2.z)
		b=Cuboid(Vector3(sp.x,sp.y,sp.z),Vector3(sv.x,sv.y,sv.z))
		Childs(7)=New OctTree(b,this.scapacity)



		this.subdivided=TRUE
		'this.capacity=1
		'Print this.capacity

		'remove point from this cuboid and insert in the child
		If this.points.count>0 Then
			while (this.points.count>0)
				Dim p1 As Vector3 Ptr=this.points.removeLast()
				For i As Long=0 To 7
					'If this.Childs(i)<>NULL Then
						this.Childs(i)->insert(p1)
					'EndIf
				Next
				'Delete p1
				'p1=NULL
			Wend
			'		EndIf
			'Childs[0].insert(p)
			'Childs[1].insert(p)
			'Childs[2].insert(p)
			'Childs[3].insert(p)
			'Childs[4].insert(p)
			'Childs[5].insert(p)
			'Childs[6].insert(p)
			'Childs[7].insert(p)
		EndIf
	EndIf
	If this.subdivided=TRUE Then

		'insert point in child
		For i As Long=0 To 7
			'If Childs(i)<>NULL Then
				this.Childs(i)->insert(p)
			'EndIf
		Next
	EndIf
End Sub
Sub OctTree.getPointsInCube(arr As arrayList Ptr,bound As Cuboid)
	If (this.bounds.CubeOverlapsCube(bound)) Then
		'Else
		If Not(this.subdivided) Then
			For i As Long =0 To this.points.count-1
				Dim p As Vector3 Ptr=points.Get(i)
				If bound.PointIsInsideCube(*p) Then
					arr->Add(p)
				EndIf
			Next
		Else
				For i As Long = 0 To 7
			If Childs(i)<>NULL Then
					Childs(i)->getPointsInCube(arr,bound)
				EndIf
			Next
		endif
	EndIf

End Sub
Sub OctTree.getPointsOnCube(arr As arrayList Ptr,bound As Cuboid,t As Single=1)
	If (this.bounds.CubeOverlapsCube(bound)) Then
		'Else
		If Not(this.subdivided) Then
			For i As Long =0 To this.points.count-1
				Dim p As Vector3 Ptr=points.Get(i)
				If bound.PointIsOnCube(*p,t) Then
					arr->Add(p)
				EndIf
			Next
		Else
				For i As Long = 0 To 7
			If Childs(i)<>NULL Then
					Childs(i)->getPointsOnCube(arr,bound,t)
			EndIf
				Next
		endif
	EndIf

End Sub
Sub OctTree.getPointsOutCube(arr As arrayList Ptr,bound As Cuboid)
	If (this.bounds.CubeOverlapsCube(bound)) Then
		'Else
		For i As Long =0 To this.points.count-1
			Dim p As Vector3 Ptr=points.Get(i)
			If Not(bound.PointIsInsideCube(*p)) Then
				arr->add(p)
			EndIf
		Next
			For i As Long = 0 To 7
		If Childs(i)<>NULL Then
				Childs(i)->getPointsOutCube(arr,bound)
		EndIf
			Next
	EndIf

End Sub
Sub OctTree.getPointsInSphere(arr As arrayList Ptr,sp As vector3,r As single)
	If (this.bounds.SphereOverlapsCube(sp,r)) Then
		'Else
		If Not(this.subdivided) Then
			For i As Long =0 To this.points.count-1
				Dim p As Vector3 Ptr=points.Get(i)
				If This.bounds.PointIsInsideSphere(*p,sp,r) Then
					arr->add(p)
				EndIf
			Next
		else
				For i As Long = 0 To 7
			If Childs(i)<>NULL Then
					Childs(i)->getPointsInSphere(arr,sp,r)
			EndIf
				Next
		EndIf
	EndIf

End Sub
Sub OctTree.getPointsOnSphere(arr As arrayList Ptr,sp As vector3,r As Single,t As Single=.1)
	If (this.bounds.SphereOverlapsCube(sp,r)) Then
		'Else
		If Not(this.subdivided) Then
			For i As Long =0 To this.points.count-1
				Dim p As Vector3 Ptr=points.Get(i)
				If This.bounds.PointIsOnSphere(*p,sp,r,t) Then
					arr->add(p)
				EndIf
			Next
		else
				For i As Long = 0 To 7
			If Childs(i)<>NULL Then
					Childs(i)->getPointsOnSphere(arr,sp,r,t)
			EndIf
				Next
		EndIf
	EndIf

End Sub
Sub OctTree.getPointsOutSphere(arr As arrayList Ptr,sp As vector3,r As Single)
	If (this.bounds.SphereOverlapsCube(sp,r)) Then
		'Else
		If Not(this.subdivided) Then
			For i As Long =0 To this.points.count-1
				Dim p As Vector3 Ptr=points.Get(i)
				If Not(This.bounds.PointIsInsideSphere(*p,sp,r)) Then
					arr->add(p)
				EndIf
			Next
		else
				For i As Long = 0 To 7
			If Childs(i)<>NULL Then
					Childs(i)->getPointsOutSphere(arr,sp,r)
			EndIf
				Next
		EndIf
	EndIf

End Sub
sub OctTree.getPointsInCylinder(arr As arrayList Ptr,co As vector3,cd As Vector3,length As Single,r As Single)
	If (this.bounds.CylinderOverlapsCube(co,cd,length,r)) Then
		If Not(this.subdivided) Then
			'Else
			For i As Long =0 To this.points.count-1
				Dim p As Vector3 Ptr=points.Get(i)
				If This.bounds.PointIsInsideCylinder(*p,co,cd,length,r)<-1 Then
					arr->add(p)
				EndIf
			Next
		else
				For i As Long = 0 To 7
			If Childs(i)<>NULL Then
					Childs(i)->getPointsInCylinder(arr,co,cd,length,r)
			EndIf
				Next
		EndIf
	EndIf

End Sub
sub OctTree.getPointsOnPlane(arr As arrayList Ptr,n As vector3,d As Single=1)
	If (this.bounds.PlaneOverlapsCube(n,d)) Then
		'Else
		If Not(this.subdivided) Then
			For i As Long =0 To this.points.count-1
				Dim p As Vector3 Ptr=points.Get(i)
				Dim s As Single=This.bounds.PointIsOnPlane(*p,n,d)
				If (s+1>0) And (s-1<0) Then
				arr->add(p)
				EndIf
			Next
		else
				For i As Long = 0 To 7
			If Childs(i)<>NULL Then
					Childs(i)->getPointsOnPlane(arr,n,d)
			EndIf
				Next
		EndIf
	EndIf
End Sub

sub OctTree.getPointsOnRay(arr As arrayList Ptr,origin As vector3,direction As vector3,r As Single=0.00001)
	If (this.bounds.RayOverlapsCube(origin,direction,r)) Then
		'Else
		If Not(this.subdivided) Then
			For i As Long =0 To this.points.count-1
				Dim p As Vector3 Ptr=points.Get(i)
				If This.bounds.PointIsOnRay(*p,origin,direction,r) Then
					arr->add(p)
				EndIf
			Next
		else
				For i As Long = 0 To 7
			If Childs(i)<>NULL Then
					Childs(i)->getPointsOnRay(arr,origin,direction,r)
			EndIf
				Next
		EndIf
	EndIf
End Sub

Sub OctTree.render()
	If (this.subdivided=TRUE) Then
		For i As Long=0 To 7
			'If (this.Childs(i)<>NULL) Then
				this.Childs(i)->render()
			'EndIf
		Next
	ElseIf(this.subdivided=FALSE) Then
		DrawCubeWiresV(this.bounds.posi,this.bounds.size,RayColor(0,0,0,16))
		'If this.points.count>0 Then
		'	For i As Long=0 To this.points.count-1
		'		Dim p As Vector3 Ptr=this.points.Get(i)
		'		DrawCube(*p,.5,.5,.5,RayColor(0,0,0,64))
		'	Next
		'EndIf
	EndIf
	'Print this.points.count
End Sub
