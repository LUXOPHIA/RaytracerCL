#ifndef RAYTRACE_CORE_CL
#define RAYTRACE_CORE_CL
//############################################################################## ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TPix
// ピクセル

typedef struct
{
  int2   Siz;  // 総数
  int2   Pos;  // 位置
  uint4  See;  // 乱数シード
  float4 Rad;  // 輝度
  float3 Col;  // 色
}
TPix;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEye
// 視点

typedef struct
{
  float3 Pos;  // 位置
}
TEye;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TScr
// スクリーン上のピクセル

typedef struct
{
  float2 Siz;  // サイズ
  float3 Pos;  // 位置
}
TScr;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TCam
// カメラ

typedef struct
{
  TSingleM4 Mov;  // 姿勢
}
TCam;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRay
// レイ

typedef struct
{
  float3 Pos;  // 出射位置
  float3 Vec;  // ベクトル
  float3 Wei;  // ウェイト
  float3 Rad;  // 輝度
}
TRay;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TTap
// 物体毎のレイの衝突点

typedef struct
{
  float  Dis;  // 衝突点までの距離
  float3 Pos;  // 衝突点の位置
  float3 Nor;  // 衝突点の法線
}
TTap;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% THit
// 最も近いレイの衝突点

typedef struct
{
  float  Dis;  // 衝突点までの距離
  float3 Pos;  // 衝突点の位置
  float3 Nor;  // 衝突点の法線
  int    Mat;  // 衝突点の材質
}
THit;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Reflect
// 完全反射

float3 Reflect( const float3 RayV,
                const float3 NorV )
{
  return RayV - 2.0f * ( dot( RayV, NorV ) * NorV );
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fresnel

float Fresnel( const float3 RayV,
               const float3 NorV,
               const float  IOR0,
               const float  IOR1 )
{
  float C = dot( RayV, NorV );

  float C2  = Pow2( C );
  float I2  = Pow2( IOR1/IOR0 );
  float CI2 = C * I2;
  float G   = sqrt( C2 + I2 - 1 );
  return ( Pow2( ( C   + G ) / ( C   - G ) )         // (G+C)/(G-C)
         + Pow2( ( CI2 + G ) / ( CI2 - G ) ) ) / 2;

  // float R0 = Pow2( ( IOR0 - IOR1 ) / ( IOR0 + IOR1 ) );
  // return R0 + ( 1 - R0 ) * pow( 1 + C, 5 );
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Refract
// 完全屈折

float3 Refract( const float3 RayV,
                const float3 NorV,
                const float  IOR0,
                const float  IOR1 )
{
  float C  = dot( RayV, NorV );
  float C2  = Pow2( C );
  float I2 = Pow2( IOR1/IOR0 );
  float G  = sqrt( C2 + I2 - 1 );

  return IOR0/IOR1 * ( RayV - ( C + G ) * NorV );
}

//############################################################################## ■
#endif
