#Ifndef __math__
#define __math__
/'
      math, geometry and other useful numerical stuff

      10/7/2017: removed all OpenGL-related stuff and the x86 assembler implementations of
         floating-point modulus operator, and moved them to their own files (again).
   '/

'' some convenience macros
#define max( a, b )                  iif( a > b, a, b )
#define min( a, b )                  iif( a < b, a, b )
'#define clamp( mn, mx, v )      iif( v < mn, mn, iif( v > mx, mx, v ) )
#define wrap( wrapValue, v )   ( ( v ) + wrapValue ) mod wrapValue

'' useful constants
'const as double pi = 4 * atn( 1 )
'const as double twoPi = 2 * pi
'const as double halfPi = pi / 2

'const as double degToRad = pi / 180
'Const as double radToDeg = 180 / pi
const as double epsilon = 0.00000001
Const As Double euler = 2.71828182845904523536028747135266249775724709369995
'' used to express angles in another unit
#define radians( ang )   ang * degToRad
#define degrees( ang )   ang * radToDeg

'' functions to return a delimited random value (uses FB implementation which
'' is a Mersenne Twister, can't remember the classification
declare function rndRange overload( byval as integer, byval as integer ) as integer
declare function rndRange( byval as single, byval as single ) as single
declare function rndRange( byval as double, byval as double ) as double
Declare function sigmoid( byval as double, byval as double ,ByVal As Double) as Double'range -8 - +8
Declare Function map(x As Double, in_min As Double, in_max As Double, out_min As Double, out_max As double) As Double
Declare Function density(x As Double,y As Double=0.0,o As Double=1.0) As Double'range -4 - +4
Declare Function distance3d(x1 As Double,y1 As Double,z1 As double, x2 As Double,y2 As Double,z2 As double) As Double
Declare Function man_distance3d(x1 As Double,y1 As Double,z1 As double, x2 As Double,y2 As Double,z2 As double) As Double
Declare Function CurveValue(newvalue As Double, oldvalue As Double, increments As Double ) As Double

Declare Function Temperature2RGB(kelvin As Double,alph As UInteger=255) As UInteger

public function rndRange( byval mn as integer, byval mx as integer ) as integer
	return int( rnd() * ( mx + 1 - mn ) + mn )
end function

public function rndRange( byval mn as single, byval mx as single ) as single
	return rnd() * ( mx - mn ) + mn
end function

public function rndRange( byval mn as double, byval mx as double ) as double
	return rnd() * ( mx - mn ) + mn
end function
public function sigmoid( byval mn as double, byval mx as Double,mo As Double ) as Double
	'Return 1.0/(1.0+(mn^2/mx+mo))
	Return 1.0/(1.0+((mn^2)/(mx+mo)))
	'  	Return 1.0/(1.0+(mn/mx+mo)^2)
	 ' 	Return 1.0/(1.0+Exp(-(mn/mx+mo)))
End Function
Public Function map(x As Double, in_min As Double, in_max As Double, out_min As Double, out_max As double) As double
	return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min
End Function
Public Function density(x As Double,y As Double=0.0,o As Double=1.0) As Double
	Return (1.0/(Sqr(2*3.14*(o*o))))*Exp(-(((x-y)*(x-y))/(2*(o*o))))
End Function
Public Function distance3d(x1 As Double,y1 As Double,z1 As double, x2 As Double,y2 As Double,z2 As Double) As Double
	Return sqr(((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) + (z2-z1)* (z2-z1)))
End Function
public Function man_distance3d(x1 As Double,y1 As Double,z1 As double, x2 As Double,y2 As Double,z2 As Double) As Double
	Return abs(((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) + (z2-z1)* (z2-z1)))
End Function
' Smoothcam
Public Function CurveValue(newvalue As Double, oldvalue As Double, increments As Double ) As Double
	If increments>1.0 Then oldvalue=oldvalue-(oldvalue-newvalue)/increments
	If increments<=1.0 Then oldvalue=newvalue

		Return oldvalue
End Function

Public Function Temperature2RGB(kelvin As Double,alph As UInteger=255) As UInteger

	Dim As double temperature = kelvin / 100.0
	Dim As Integer red0, green0, blue0

	If (temperature < 66.0) Then
		red0 = 255
	Else
		'// a + b x + c Log[x] /.
		'// {a -> 351.97690566805693`,
		'// b -> 0.114206453784165`,
		'// c -> -40.25366309332127
		'//x -> (kelvin/100) - 55}
'		red0 = temperature - 55.0
'		red0 = 351.97690566805693+ 0.114206453784165 * red0 - 40.25366309332127 * Log(red0)
		red0 = temperature - 60.0
		red0 = 329.698727466+ 0.114206453784165 * red0 - 40.25366309332127 * Log(red0)
		If (red0 < 0) Then red0 = 0
		If (red0 > 255) Then red0 = 255
	EndIf

	'/* Calculate green0 */

	if (temperature <= 66.0) Then

		'// a + b x + c Log[x] /.
		'// {a -> -155.25485562709179`,
		'// b -> -0.44596950469579133`,
		'// c -> 104.49216199393888`,
		'// x -> (kelvin/100) - 2}
'		green0 = temperature - 2
'		green0 = -155.25485562709179 - 0.44596950469579133 * green0 + 104.49216199393888 * Log(green0)
		green0 = temperature
		green0 =  (99.4708025861 * log(green0)) - 161.1195681661
		If (green0 < 0) Then green0 = 0
		If (green0 > 255) Then green0 = 255

	else

		'// a + b x + c Log[x] /.
		'// {a -> 325.4494125711974`,
		'// b -> 0.07943456536662342`,
		'// c -> -28.0852963507957`,
		'// x -> (kelvin/100) - 50}
'		green0 = temperature - 50.0
'		green0 = 325.4494125711974 + 0.07943456536662342 * green0 - 28.0852963507957 * Log(green0)
		green0 = temperature - 60.0
		green0 = 288.1221695283 + 0.07943456536662342 * green0 - 28.0852963507957 * Log(green0)
		If (green0 < 0) Then green0 = 0
		If (green0 > 255) Then green0 = 255

	endif

	'  /* Calculate blue0 */

	if (temperature >= 66.0) Then
		blue0 = 255
	else

		if (temperature <= 19.0) Then
			blue0 = 0
		else

			'// a + b x + c Log[x] /.
			'// {a -> -254.76935184120902`,
			'// b -> 0.8274096064007395`,
			'// c -> 115.67994401066147`,
			'// x -> kelvin/100 - 10}
'			blue0 = temperature - 10
'			blue0 = -254.76935184120902 + 0.8274096064007395 * blue0 + 115.67994401066147 * log(blue0)
			blue0 = temperature - 10
			blue0 = (138.5177312231 * log(blue0)) - 305.0447927307
			If (blue0 < 0) Then blue0 = 0
			If (blue0 > 255) Then blue0 = 255
		EndIf
	EndIf

	return RGBA(red0,green0,blue0,alph)
End Function
type a_t
declare function b(a as integer) as integer
as integer a
end Type
'2.9 x 10 ^ 6 Kelvins per nanometer / temperature = wavelength
/'inline rgb_t convert_wave_length_nm_to_rgb(const double wave_length_nm)
{
   // Credits: Dan Bruton http://www.physics.sfasu.edu/astro/color.html
   double red   = 0.0;
   double green = 0.0;
   double blue  = 0.0;

   if ((380.0 <= wave_length_nm) && (wave_length_nm <= 439.0))
   {
      red   = -(wave_length_nm - 440.0) / (440.0 - 380.0);
      green = 0.0;
      blue  = 1.0;
   }
   else if ((440.0 <= wave_length_nm) && (wave_length_nm <= 489.0))
   {
      red   = 0.0;
      green = (wave_length_nm - 440.0) / (490.0 - 440.0);
      blue  = 1.0;
   }
   else if ((490.0 <= wave_length_nm) && (wave_length_nm <= 509.0))
   {
      red   = 0.0;
      green = 1.0;
      blue  = -(wave_length_nm - 510.0) / (510.0 - 490.0);
   }
   else if ((510.0 <= wave_length_nm) && (wave_length_nm <= 579.0))
   {
      red   = (wave_length_nm - 510.0) / (580.0 - 510.0);
      green = 1.0;
      blue  = 0.0;
   }
   else if ((580.0 <= wave_length_nm) && (wave_length_nm <= 644.0))
   {
      red   = 1.0;
      green = -(wave_length_nm - 645.0) / (645.0 - 580.0);
      blue  = 0.0;
   }
   else if ((645.0 <= wave_length_nm) && (wave_length_nm <= 780.0))
   {
      red   = 1.0;
      green = 0.0;
      blue  = 0.0;
   }

   double factor = 0.0;

   if ((380.0 <= wave_length_nm) && (wave_length_nm <= 419.0))
      factor = 0.3 + 0.7 * (wave_length_nm - 380.0) / (420.0 - 380.0);
   else if ((420.0 <= wave_length_nm) && (wave_length_nm <= 700.0))
      factor = 1.0;
   else if ((701.0 <= wave_length_nm) && (wave_length_nm <= 780.0))
      factor = 0.3 + 0.7 * (780.0 - wave_length_nm) / (780.0 - 700.0);
   else
      factor = 0.0;

   rgb_t result;

   const double gamma         =   0.8;
   const double intensity_max = 255.0;

   #define round(d) std::floor(d + 0.5)

   result.red   = static_cast<unsigned char>((red   == 0.0) ? red   : round(intensity_max * std::pow(red   * factor, gamma)));
   result.green = static_cast<unsigned char>((green == 0.0) ? green : round(intensity_max * std::pow(green * factor, gamma)));
   result.blue  = static_cast<unsigned char>((blue  == 0.0) ? blue  : round(intensity_max * std::pow(blue  * factor, gamma)));

   #undef round

   return result;
}'/

Function smoothStep(x As Double,t1 As Double, t2 As double) As Double
	Dim k As Double= max(0,min(1,((x-t1)/(t2-t1))))
	Dim s As Double=(k*k)*(3-(2*k))
	Return s
End Function
#endif
