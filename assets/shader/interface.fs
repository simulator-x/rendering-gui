#version 150
precision highp float;

uniform sampler2D jvr_Texture0;
uniform sampler2D jvr_Texture1;

uniform float alpha;

in vec2 texCoord;

out vec4 final_color;

void main (void){
    vec4 overlay = texture(jvr_Texture1, texCoord);
    final_color = mix(texture(jvr_Texture0, texCoord), vec4(overlay.xyz, alpha), overlay.a);
}
