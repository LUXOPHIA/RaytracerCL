#ifndef RAYTRACE_OBJECT_CL
#define RAYTRACE_OBJECT_CL
//############################################################################## ■

#include<Math.cl>
#include<Raytrace.core.cl>

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ObjGround
// 地面

bool ObjGround( const TRay* Ray,
                TTap* const Tap )
{
  if ( 0 <= Ray->Vec.y ) return false;  // 交差なし

  float t = ( Ray->Pos.y - -1.001 ) / -Ray->Vec.y;

  if ( t <= 0 ) return false;  // 交差なし

  Tap->Dis = t;
  Tap->Pos = Ray->Pos + t * Ray->Vec;
  Tap->Nor = (float3)( 0, 1, 0 );

  return true;  // 交差あり
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ObjSphere
// 球体

bool ObjSphere( const TRay* Ray,
                TTap* const Tap )
{
  float B = dot( Ray->Pos, Ray->Vec );
  float C = Length2( Ray->Pos ) - 1;

  float D = Pow2( B ) - C;

  if ( D <= 0 ) return false;  // 交差なし

  float t = -B - sign( C ) * sqrt( D );

  if ( t <= 0 ) return false;  // 交差なし

  Tap->Dis = t;
  Tap->Pos = Ray->Pos + t * Ray->Vec;
  Tap->Nor = Tap->Pos;

  return true;  // 交差あり
}

//############################################################################## ■
#endif
