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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Rand

uint rotl( const uint x, const int k )
{
  return ( x << k ) | ( x >> ( 32 - k ) );
}

float Rand( uint4* const Seed )
{
  const uint Result = rotl( Seed->x * 5, 7 ) * 9;

  const uint t = Seed->y << 9;

  Seed->z ^= Seed->x;
  Seed->w ^= Seed->y;
  Seed->y ^= Seed->z;
  Seed->x ^= Seed->w;

  Seed->z ^= t;

  Seed->w = rotl( Seed->w, 11 );

  return (float)( Result ) / 4294967296.0;
}

//############################################################################## ■
#endif
