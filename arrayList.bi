#Ifndef __arrayList__
#define __arrayList__

#include once "platform.bi"
#include once "core.bi"
#Include once "math.bi"
#include once "crt.bi" '' needed for memcpy

/'
      simple java-like ArrayList class

      responsibility: represents a collection of objects, accessed and indexed
         as an array
      mutability: immutable

      note that these classes aren't meant to do garbage collection,
         merely to conveniently index and traverse a collection of objects
      also the classes aren't optimized, as they are meant to be
         used as a reference implementation (there's almost no error handling)
   '/
class ArrayList extends object 'inherits ICollection
'' public interface
public:
declare constructor()

declare function get( byval index as uint64 ) as any ptr
declare function count() as uint64
declare sub add( byval e as any ptr )
declare sub insert( byval e as any ptr, byval before as uint64 )
declare Sub Swapa( byval i1 as uint64,byval i2 as uint64 )
'Declare Sub resizeElements( v As Integer)
declare function remove( byval index as uint64 ) as any ptr
declare function remove( byval item as any ptr ) as any ptr
declare function removeLast() as any ptr
declare function removeFirst() as any ptr

'' state members
protected:
as any ptr   m_element( any )
as uint64      m_count = any

/'
         convenience macro for brevity
         this macro resizes the m_element array by v elements, either positive or negative
            if v is positive, the array expands
            if v is negative, the array shrinks

         the code uses +1 and -1 for further clarify the meaning of the expression
      '/
	#macro resizeElements( v )
		m_count += v :
		ReDim preserve m_element( 0 to m_count - 1 )
	#EndMacro
end class
'Sub ArrayList.resizeElements(v As integer)
'	m_count+=v
'	If m_count>UBound(m_element) Then
'		ReDim preserve m_element( 0 to m_count-1 )
'	ElseIf m_count<UBound(m_element)-1 Then
'		ReDim preserve m_element( 0 to m_count-1 )
'	EndIf
'End Sub
constructor ArrayList()
m_count = 0
End constructor

function ArrayList.count() as uint64
	return( m_count )
end function

function ArrayList.get( byval index as uint64 ) as any ptr
	/'
         gets the element of the list indicated by index
            note that there is no error checking on this one, for this can
            mask out of bound errors
         if the list tries to access an item out of bounds, the application deserves to
         fail miserably
      '/
	return( m_element( index ) )
end function

sub ArrayList.add( byval e as any ptr )
	resizeElements( +1 )

	m_element( m_count - 1 ) = e
end sub

Sub ArrayList.swapa( byval i1 as uint64,byval i2 as uint64 )
	Swap m_element( i1 ), m_element( i2 )
End Sub
sub ArrayList.insert( byval e as any ptr, byval before as uint64 )
	'' don't allow insertion out of bounds
	before = min( m_count, max( 0, before ) )

	'' trivial case, inserting at the end of the list
	if( before = m_count - 1 ) then
		resizeElements( +1 )

		m_element( m_count - 1 ) = m_element( m_count - 2 )
		m_element( m_count - 2 ) = e
	else
		resizeElements( +1 )

		'' calculate number of elements to move
		dim as uint64 elem = m_count - 1 - before
		'' and move them to make space for the inserted item
		memcpy( @m_element( before + 1 ), @m_element( before ), elem * sizeOf( any ptr ) )

		m_element( before ) = e
	end if
end sub

function ArrayList.remove( byval item as any ptr ) as any ptr
	'' removes an item from the list by pointer
	dim as any ptr ret = 0
	dim as int64 elem = -1 '' assume not found

	'' search it (inefficiently)
	for i as uint64 = 0 to m_count - 1
		if( item = m_element( i ) ) then
			'' found it
			elem = i
			exit for
		end if
	next

	'' if found, return it, else return nothing
	if( elem <> -1 ) then
		return( remove( elem ) )
	else
		return( nothing )
	end if
end function

function ArrayList.remove( byval index as uint64 ) as any ptr
	'' removes an item from the list by index
	'' don't allow removal out of bounds
	index = min( m_count - 1, max( 0, index ) )

	dim as any ptr ret = m_element( index )

	if( index = m_count - 1 ) then
		'' trivial removal, the last item
		resizeElements( -1 )

		return( ret )
	end if

	'' only 2 elements to remove remaining
	if( index = 0 and m_count < 3 ) then
		m_element( 0 ) = m_element( 1 )

		resizeElements( -1 )

		return( ret )
	end if

	'' general case (elements > 2)
	'' number of elements to move
	dim as uint64 elem = m_count - 1 - index
	'' move the rest of the elements
	memcpy( @m_element( index ), @m_element( index + 1 ), elem * sizeOf( any ptr ) )

	resizeElements( -1 )

	return( ret )
end function


function ArrayList.removeLast() as any ptr
	'' convenience function for removing the last element of the list
	dim as any ptr ret = m_element( m_count - 1 )

	resizeElements( -1 )

	return( ret )
end function

function ArrayList.removeFirst() as any ptr
	'' convenience function for removing the first element of the list
	dim as any ptr ret = m_element( 0 )

	'' there's only one element remaining
	if( m_count = 1 ) then
		m_count -= 1
		redim m_element( 0 )

		return( ret )
	end if

	'' there's 2 elements remaining
	if( m_count = 2 ) then
		m_element( 0 ) = m_element( 1 )

		resizeElements( -1 )

		return( ret )
	end if

	'' general case
	'' calculate number of elements to move
	dim as uint64 elem = m_count - 1

	'' move the remaining elements to their new position
	memcpy( @m_element( 0 ), @m_element( 1 ), elem * sizeOf( any ptr ) )

	'' and resize the member array
	resizeElements( -1 )

	return( ret )
end function
#endif

/'
   '' some code for unit testing

   class test extends object
      public:
         as integer value = any

         declare constructor()
         declare constructor( byval v as integer )
         declare destructor()
   end class

   constructor test()
      value = 0
   end constructor

   constructor test( byval v as integer )
      value = v
   end constructor

   destructor test()
      print "Destructor "; value; " called."
   end destructor

   dim as ArrayList al
   dim as test ptr t
   dim as integer numElements = 10
   dim as any ptr what

   for i as integer = 1 to numElements
      dim as test ptr t = new test( i )
      al.add( t )

      if i = 5 then
         what = t
      end if
   next

   al.insert( new test( 343 ), al.count - 2 )

   t = al.remove( 8798 )
   delete t

   t = al.removeLast()
   delete t

   t = al.remove( what )
   delete t

   '' show the elements
   for i as integer = 0 to al.count - 1
      t = al.get( i )

      print t->value
   next

   '' delete all elements
   do while al.count > 0
      t = al.removeFirst

      delete( t )
      t = 0
   loop

   '' this code should never be executed if everything went ok
   for i as integer = 0 to al.count - 1
      t = al.get( i )

      print t->value
   next

   sleep()
'/
