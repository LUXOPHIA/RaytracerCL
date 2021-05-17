#ifndef MATH_CL
#define MATH_CL
//############################################################################## ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

const double Pi  = 3.141592653589793;                                           // π
const double Pi2 = Pi * 2.0;                                                    // 2π
const double P2i = Pi / 2.0;                                                    // π/2

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% pow2
// pown( X, 2 )

float Pow2( const float X )
{
  return X * X;
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VecToSky
// Pow2( length( V ) )

float Length2( const float3 V )
{
  return dot( V, V );
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VecToSky
// 変換：ベクトル → 球面座標

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

float Random( uint4* const See )
{
  const uint Result = rotl( See->x * 5, 7 ) * 9;

  const uint t = See->y << 9;

  See->z ^= See->x;
  See->w ^= See->y;
  See->y ^= See->z;
  See->x ^= See->w;

  See->z ^= t;

  See->w = rotl( See->w, 11 );

  return as_float( Result & 0x007FFFFFu | 0x3F800000u ) - 1;
}

//############################################################################## ■
#endif
