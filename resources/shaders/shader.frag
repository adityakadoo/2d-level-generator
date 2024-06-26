#version 430 core

// Interpolated values from the vertex shaders
in vec3 clr;
in vec2 uv;
uniform sampler2D tex_00;

// Ouput data
out vec4 fColor;

void main()
{
  fColor = vec4( texture(tex_00, uv).rgb * clr
               , 1.0 );
}
