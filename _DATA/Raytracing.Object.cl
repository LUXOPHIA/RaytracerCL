#ifndef RAYTRACING_OBJECT_CL
#define RAYTRACING_OBJECT_CL
//############################################################################## ■

#include<Math.cl>
#include<Raytracing.cl>

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ObjPlain
// 地面


const int MAX_MARCHING_STEPS = 16;
const float EPSILON = 0.001;
const float MAX_DIST =

bool ObjPlane( const TRay* Ray,
               TTap* const Tap )
{
  const float Z = -1;

  if ( 0 <= Ray->Vec.y ) return false;  // 交差なし

  Tap->Dis = ( Z - Ray->Pos.y ) / Ray->Vec.y;

  if ( Tap->Dis <= 0 ) return false;  // 交差なし

  Tap->Pos = Tap->Dis * Ray->Vec + Ray->Pos;
  Tap->Nor = (float3)( 0, 1, 0 );

  return true;  // 交差あり
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ObjSpher
// 球体

bool ObjSpher( const  TRay* Ray0,
               TTap*  const Tap,
               TSingleM4 const Mov )
{
  TRay Ray = *Ray0;

  TSingleM4 Inv = Inverse( Mov );

  Ray.Pos = MulPos( Inv, Ray.Pos );
  Ray.Vec = MulVec( Inv, Ray.Vec );
  float VecL = length( Ray.Vec );  
  Ray.Vec /= VecL;

  float B = dot( Ray.Pos, Ray.Vec );
  float C = Length2( Ray.Pos ) - 1;
  float D = Pow2( B ) - C;

  if ( D <= 0 ) return false;  // 交差なし

  Tap->Dis = -B - sign( C ) * sqrt( D );

  if ( Tap->Dis <= 0 ) return false;  // 交差なし

  Tap->Pos = Tap->Dis * Ray.Vec + Ray.Pos;
  Tap->Nor = Tap->Pos;

  Tap->Dis /= VecL;
  Tap->Pos = MulPos( Mov, Tap->Pos );
  Tap->Nor = normalize( MulVec( Trans( Inv ), Tap->Nor ) );

  return true;  // 交差あり
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ObjField
// 距離場

float GetDis( const float3 P )
{
  return length( P ) - 1;
}

 /**
 * Signed distance function for a sphere centered at the origin with radius r.
 */
float sphereSDF(float3 p, float r) {
  return length(p) - r;
}

// polynomial smooth min (k = 0.1);
float smin( float a, float b, float k )
{
  float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
  return mix( b, a, h ) - k*h*(1.0-h);
}

float shortestDistanceToSurface(float3 eye, float3 marchingDirection, float start, float end) {
  float depth = start;
  for (int i = 0; i < MAX_MARCHING_STEPS; i++) {
    float dist = sceneSDF(eye + depth * marchingDirection);
    if (dist < EPSILON) {
      return depth;
    }
    depth += dist;
    if (depth >= end) {
      return end;
    }
  }
  return end;
}

/**
 * Signed distance function describing the scene.
 */
float sceneSDF(float3 samplePoint) {
  float ballRadius = 1.0;
  float balls = MAX_DIST;
  for (int i = 1; i < 1109; i ++) {
      balls = smin(balls, sphereSDF(samplePoint + pos[i], ballRadius), 0.7);
  }

  return balls;
}


//------------------------------------------------------------------------------

float3 GetNor( const float3 P )
{
  const float3 Xd = { FLOAT_EPS2, 0, 0 };
  const float3 Yd = { 0, FLOAT_EPS2, 0 };
  const float3 Zd = { 0, 0, FLOAT_EPS2 };

  float3 Result;

  Result.x = sceneSDF( P + Xd ) - sceneSDF( P - Xd );
  Result.y = sceneSDF( P + Yd ) - sceneSDF( P - Yd );
  Result.z = sceneSDF( P + Zd ) - sceneSDF( P - Zd );

  return normalize( Result );
}


//------------------------------------------------------------------------------

bool ObjField( const TRay* Ray,
               TTap* const Tap )
{
  Tap->Dis = 0;
  for( int i = 0; i < 64; i++ )
  {
    Tap->Pos = Tap->Dis * Ray->Vec + Ray->Pos;

    float D = fabs( GetDis( Tap->Pos ) );

    Tap->Dis += D;

    if ( D < FLOAT_EPS2 )
    {
      Tap->Nor  = GetNor( Tap->Pos );
      Tap->Pos -= D * Tap->Nor;

      return true;  // 交差あり         F
    }
  }

  return false;  // 交差なし
}

//############################################################################## ■
#endif
