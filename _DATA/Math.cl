#ifndef MATH_CL
#define MATH_CL
//############################################################################## ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

constant double Pi  = 3.141592653589793;                                        // π
constant double Pi2 = 3.141592653589793 * 2.0;                                  // 2π
constant double P2i = 3.141592653589793 / 2.0;                                  // π/2

constant float FLOAT_EPS  = 1.1920928955078125E-7;                              // float 型の計算機イプシロン
constant float FLOAT_EPS1 = 1.1920928955078125E-6;
constant float FLOAT_EPS2 = 1.1920928955078125E-5;
constant float FLOAT_EPS3 = 1.1920928955078125E-4;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% pow2
// pown( X, 2 )

float Pow2( const float X )
{
  return X * X;
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Length2
// Pow2( length( V ) )

float Length2( const float3 V )
{
  return dot( V, V );
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VecToSky
// Unit Vector to Spherical Coordinates

float2 VecToSky( const float3 Vec )
{
  float2 Result;

  Result.x = ( Pi - atan2( -Vec.x, -Vec.z ) ) / Pi2;
  Result.y =        acos (  Vec.y           ) / Pi ;

  return Result;
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Random

uint rotl( const uint x, const int k )
{
  return ( x << k ) | ( x >> ( 32 - k ) );
}

float Rand( uint4* const See )
{
  const uint Result = rotl( See->x * 5, 7 ) * 9;

  const uint t = See->y << 9;

  See->z ^= See->x;
  See->w ^= See->y;
  See->y ^= See->z;
  See->x ^= See->w;

  See->z ^= t;

  See->w = rotl( See->w, 11 );

  return as_float( ( Result & 0x007FFFFFu ) | 0x3F800000u ) - 1;
}

//############################################################################## ■
#endif

