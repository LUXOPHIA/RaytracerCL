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


// const int MAX_MARCHING_STEPS = 16;
// const float EPSILON = 0.001;
// const float MAX_DIST =      999999.f;

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
float SphereSDF(float3 p, float3 c, float r) {
  return length(p - c) - r;
}

// polynomial smooth min (k = 0.1);
float Smin( float a, float b, float k )
{
  float h = clamp( 0.5f+0.5f*(b-a)/k, 0.f, 1.f );
  return mix( b, a, h ) - k*h*(1.f-h);
}

/**
 * Signed distance function describing the scene.
 */
float SceneSDF(float3 samplePoint, global TShaper* spheres) {
  float ballRadius = 0.7f;
  float Result = MAXFLOAT;

  for (int i = 1; i < 1109; i ++) {
      TSingleM4 M = spheres[ i ].Mov;
      float3    C = (float3)( M._14, M._24, M._34 );
      float     R = M._11;
      float D = SphereSDF( samplePoint, C, R );
      Result = Smin(Result, D, 0.1f);
  }

  return Result;
}



//------------------------------------------------------------------------------

float3 GetNor( const float3 P, global TShaper* spheres )
{
  const float3 Xd = { FLOAT_EPS2, 0, 0 };
  const float3 Yd = { 0, FLOAT_EPS2, 0 };
  const float3 Zd = { 0, 0, FLOAT_EPS2 };

  float3 Result;

  Result.x = SceneSDF( P + Xd, spheres ) - SceneSDF( P - Xd, spheres );
  Result.y = SceneSDF( P + Yd, spheres ) - SceneSDF( P - Yd, spheres );
  Result.z = SceneSDF( P + Zd, spheres ) - SceneSDF( P - Zd, spheres );

  return normalize( Result );
}


//------------------------------------------------------------------------------

bool ObjField( const TRay* Ray,
               TTap* const Tap,
               global TShaper* Spheres
               )
{
  Tap->Dis = 0;
  for( int i = 0; i < 64; i++ )
  {
    Tap->Pos = Tap->Dis * Ray->Vec + Ray->Pos;

    float D = fabs( SceneSDF( Tap->Pos, Spheres ) );

    Tap->Dis += D;

    if ( D < FLOAT_EPS2 )
    {
      Tap->Nor  = GetNor( Tap->Pos, Spheres );
      Tap->Pos -= D * Tap->Nor;

      return true;  // 交差あり
    }
  }

  return false;  // 交差なし
}

//############################################################################## ■
#endif
