'for this demo you need thr Raylib lib and Raymath.bi

#include "Raylib.bi"
#include "OctTree.bi"
Dim b As Cuboid=Cuboid(Vector3(0,0,0),Vector3(256,256,256))
Dim oc As OctTree ptr=New OctTree(b,8)
For i As Long=0 To 64000
	Dim As Vector3 Ptr p=New Vector3((0.5-Rnd)*512,(0.5-Rnd)*512,(0.5-Rnd)*512)
oc->insert(p)
Next
Dim arr As arrayList Ptr=New arrayList
Dim arr1 As arrayList Ptr=New arrayList
Dim arr2 As arrayList Ptr=New arrayList
Dim bound As Cuboid=Cuboid(Vector3((0.5-Rnd)*200,(0.5-Rnd)*200,(0.5-Rnd)*200),Vector3(100,100,100))
oc->getPointsInCube(bound,arr)
dim sp As Vector3=Vector3((0.5-Rnd)*200,(0.5-Rnd)*200,(0.5-Rnd)*200) 
oc->getPointsInSphere(sp,50,arr1)
Dim org As Vector3=Vector3((0.5-Rnd)*200,(0.5-Rnd)*200,(0.5-Rnd)*200)
Dim direc As Vector3=Vector3((0.5-Rnd)*2,(0.5-Rnd)*2,(0.5-Rnd)*2)
oc->getPointsOnRay(org,direc,arr2)
' Main game loop

	print "CubeQuerry"
	For i As Long=0 To arr->count-1
	Dim p As Vector3 Ptr=arr->get(i)
	print *p.x,*p.y,*p.z	
	Next
	Print "SphereQuerry"
	For i As Long=0 To arr1->count-1
	Dim p As Vector3 Ptr=arr1->get(i)
	print *p.x,*p.y,*p.z	
	Next
	print "RayQuerry"
	For i As Long=0 To arr2->count-1
	Dim p As Vector3 Ptr=arr2->get(i)
	print *p.x,*p.y,*p.z	
	Next
	
	
	sleep
	end