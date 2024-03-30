#version 430 core

layout(location = 0) in vec2 vPosition;
layout(location = 1) in float vColor;
layout(location = 2) in vec2 uvCoords;

// Output data ; will be interpolated for each fragment.
out vec3 clr;
out vec2 uv;

uniform mat4 transform;

void main()
{
  gl_Position = transform * vec4(vPosition, 0.0, 1.0);

// The color of each vertex will be interpolated
// to produce the color of each fragment
  clr = vec3(vColor, vColor, vColor);
	uv  = uvCoords;
}
