#ifndef MATH_MATRIX_CL
#define MATH_MATRIX_CL
//############################################################################## ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TSingleM4
// ４×４行列

typedef struct
{
  float _11, _21, _31, _41,
        _12, _22, _32, _42,
        _13, _23, _33, _43,
        _14, _24, _34, _44;
}
TSingleM4;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TSingleM4

///////////////////////////////////////////////////////////////////////// 演算子

TSingleM4 __attribute__((overloadable)) Mul( const TSingleM4 A, const TSingleM4 B )
{
  TSingleM4 Result;

  Result._11 = A._11 * B._11 + A._12 * B._21 + A._13 * B._31 + A._14 * B._41;
  Result._12 = A._11 * B._12 + A._12 * B._22 + A._13 * B._32 + A._14 * B._42;
  Result._13 = A._11 * B._13 + A._12 * B._23 + A._13 * B._33 + A._14 * B._43;
  Result._14 = A._11 * B._14 + A._12 * B._24 + A._13 * B._34 + A._14 * B._44;

  Result._21 = A._21 * B._11 + A._22 * B._21 + A._23 * B._31 + A._24 * B._41;
  Result._22 = A._21 * B._12 + A._22 * B._22 + A._23 * B._32 + A._24 * B._42;
  Result._23 = A._21 * B._13 + A._22 * B._23 + A._23 * B._33 + A._24 * B._43;
  Result._24 = A._21 * B._14 + A._22 * B._24 + A._23 * B._34 + A._24 * B._44;

  Result._31 = A._31 * B._11 + A._32 * B._21 + A._33 * B._31 + A._34 * B._41;
  Result._32 = A._31 * B._12 + A._32 * B._22 + A._33 * B._32 + A._34 * B._42;
  Result._33 = A._31 * B._13 + A._32 * B._23 + A._33 * B._33 + A._34 * B._43;
  Result._34 = A._31 * B._14 + A._32 * B._24 + A._33 * B._34 + A._34 * B._44;

  Result._41 = A._41 * B._11 + A._42 * B._21 + A._43 * B._31 + A._44 * B._41;
  Result._42 = A._41 * B._12 + A._42 * B._22 + A._43 * B._32 + A._44 * B._42;
  Result._43 = A._41 * B._13 + A._42 * B._23 + A._43 * B._33 + A._44 * B._43;
  Result._44 = A._41 * B._14 + A._42 * B._24 + A._43 * B._34 + A._44 * B._44;

  return Result;
}

float4 __attribute__((overloadable)) Mul( const TSingleM4 A, const float4 B )
{
  float4 Result;

  Result.x = A._11 * B.x + A._12 * B.y + A._13 * B.z + A._14 * B.w;
  Result.y = A._21 * B.x + A._22 * B.y + A._23 * B.z + A._24 * B.w;
  Result.z = A._31 * B.x + A._32 * B.y + A._33 * B.z + A._34 * B.w;
  Result.w = A._41 * B.x + A._42 * B.y + A._43 * B.z + A._44 * B.w;

  return Result;
}

float3 __attribute__((overloadable)) MulPos( const TSingleM4 A, const float3 B )
{
  float4 Result;

  Result.x = A._11 * B.x + A._12 * B.y + A._13 * B.z + A._14;
  Result.y = A._21 * B.x + A._22 * B.y + A._23 * B.z + A._24;
  Result.z = A._31 * B.x + A._32 * B.y + A._33 * B.z + A._34;
  Result.w = A._41 * B.x + A._42 * B.y + A._43 * B.z + A._44;

  return Result.xyz / Result.w;
}

float3 __attribute__((overloadable)) MulVec( const TSingleM4 A, const float3 B )
{
  float3 Result;

  Result.x = A._11 * B.x + A._12 * B.y + A._13 * B.z;
  Result.y = A._21 * B.x + A._22 * B.y + A._23 * B.z;
  Result.z = A._31 * B.x + A._32 * B.y + A._33 * B.z;

  return Result;
}

//############################################################################## ■
#endif