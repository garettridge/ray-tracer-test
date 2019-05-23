window.Triangle = window.classes.Triangle =
class Triangle extends Shape    // The simplest possible Shape – one triangle.  It has 3 vertices, each
{ constructor()                 // having their own 3D position, normal vector, and texture-space coordinate.
    { super( "positions", "normals", "texture_coords" );                       // Name the values we'll define per each vertex.
      this.positions      = [ Vec.of(0,0,0), Vec.of(1,0,0), Vec.of(0,1,0) ];   // Specify the 3 vertices -- the point cloud that our Triangle needs.
      this.normals        = [ Vec.of(0,0,1), Vec.of(0,0,1), Vec.of(0,0,1) ];   // ...
      this.texture_coords = [ Vec.of(0,0),   Vec.of(1,0),   Vec.of(0,1)   ];   // ...
      this.indices        = [ 0, 1, 2 ];                                       // Index into our vertices to connect them into a whole triangle.
    }
}       

window.Square = window.classes.Square =
class Square extends Shape      // A square, demonstrating shared vertices.  On any planar surface, the interior edges don't make any important seams.
{                               // In these cases there's no reason not to re-use data of the common vertices between triangles.  This makes all the
  constructor()                 // vertex arrays (position, normals, etc) smaller and more cache friendly.
    { super( "positions", "normals", "texture_coords" );                                     // Name the values we'll define per each vertex.
      this.positions     .push( ...Vec.cast( [-1,-1,0], [1,-1,0], [-1,1,0], [1,1,0] ) );     // Specify the 4 vertices -- the point cloud that our Square needs.
      this.normals       .push( ...Vec.cast( [0,0,1],   [0,0,1],  [0,0,1],  [0,0,1] ) );     // ...
      this.texture_coords.push( ...Vec.cast( [0,0],     [1,0],    [0,1],    [1,1]   ) );     // ...
      this.indices       .push( 0, 1, 2,     1, 3, 2 );                                      // Two triangles this time, indexing into four distinct vertices.
    }
}

window.Tetrahedron = window.classes.Tetrahedron =
class Tetrahedron extends Shape            // A demo of flat vs smooth shading (a boolean argument selects which one). Also our first 3D, non-planar shape.
{ constructor( using_flat_shading ) 
    { super( "positions", "normals", "texture_coords" );
      var a = 1/Math.sqrt(3);
      if( !using_flat_shading )                                         // Method 1:  A tetrahedron with shared vertices.  Compact, performs better,
      {                                                                 // but can't produce flat shading or discontinuous seams in textures.
          this.positions     .push( ...Vec.cast( [ 0, 0, 0], [1,0,0], [0,1,0], [0,0,1] ) );          
          this.normals       .push( ...Vec.cast( [-a,-a,-a], [1,0,0], [0,1,0], [0,0,1] ) );          
          this.texture_coords.push( ...Vec.cast( [ 0, 0   ], [1,0  ], [0,1, ], [1,1  ] ) );
          this.indices       .push( 0, 1, 2,   0, 1, 3,   0, 2, 3,    1, 2, 3 );  // Vertices are shared multiple times with this method.
      }
      else
      { this.positions     .push( ...Vec.cast( [0,0,0], [1,0,0], [0,1,0],         // Method 2:  A tetrahedron with 
                                               [0,0,0], [1,0,0], [0,0,1],         // four independent triangles.
                                               [0,0,0], [0,1,0], [0,0,1],
                                               [0,0,1], [1,0,0], [0,1,0] ) );

        this.normals       .push( ...Vec.cast( [0,0,-1], [0,0,-1], [0,0,-1],        // This here makes Method 2 flat shaded, since values of
                                               [0,-1,0], [0,-1,0], [0,-1,0],        // normal vectors can be constant per whole triangle.
                                               [-1,0,0], [-1,0,0], [-1,0,0],        // Repeat them for all three vertices.
                                               [ a,a,a], [ a,a,a], [ a,a,a] ) );

        this.texture_coords.push( ...Vec.cast( [0,0], [1,0], [1,1],      // Each face in Method 2 also gets its own set of texture coords
                                               [0,0], [1,0], [1,1],      //(half the image is mapped onto each face).  We couldn't do this
                                               [0,0], [1,0], [1,1],      // with shared vertices since this features abrupt transitions
                                               [0,0], [1,0], [1,1] ) );  // when approaching the same point from different directions.

        this.indices.push( 0, 1, 2,    3, 4, 5,    6, 7, 8,    9, 10, 11 );      // Notice all vertices are unique this time.
      }
    }
}

window.Windmill = window.classes.Windmill =
class Windmill extends Shape   // As our shapes get more complicated, we begin using matrices and flow
{ constructor( num_blades )   // control (including loops) to generate non-trivial point clouds and connect them.
    { super( "positions", "normals", "texture_coords" );
      for( var i = 0; i < num_blades; i++ )     // A loop to automatically generate the triangles.
        {
          var spin = Mat4.rotation( i * 2*Math.PI/num_blades, Vec.of( 0,1,0 ) );            // Rotate around a few degrees in XZ plane to place each new point.
          var newPoint  = spin.times( Vec.of( 1,0,0,1 ) ).to3();   // Apply that XZ rotation matrix to point (1,0,0) of the base triangle.
          this.positions.push( newPoint,                           // Store this XZ position.                  This is point 1.
                               newPoint.plus( [ 0,1,0 ] ),         // Store it again but with higher y coord:  This is point 2.
                                        Vec.of( 0,0,0 )    );      // All triangles touch this location.       This is point 3.

          // Rotate our base triangle's normal (0,0,1) to get the new one.  Careful!  Normal vectors are not points; their perpendicularity constraint
          // gives them a mathematical quirk that when applying matrices you have to apply the transposed inverse of that matrix instead.  But right 
          // now we've got a pure rotation matrix, where the inverse and transpose operations cancel out.
          var newNormal = spin.times( Vec.of( 0,0,1 ).to4(0) ).to3();  
          this.normals       .push( newNormal, newNormal, newNormal          );
          this.texture_coords.push( ...Vec.cast( [ 0,0 ], [ 0,1 ], [ 1,0 ] ) );
          this.indices       .push( 3*i, 3*i + 1, 3*i + 2                    ); // Procedurally connect the three new vertices into triangles.
        }
    }
}

window.Subdivision_Sphere = window.classes.Subdivision_Sphere =
class Subdivision_Sphere extends Shape  // A subdivision surface ( see Wikipedia article ) is initially simple, then builds itself into a 
{                                 // more and more detailed shape of the same layout.  Each act of subdivision makes it a better 
                                  // approximation of some desired mathematical surface by projecting each new point onto that surface's 
                                  // known implicit equation.  For a sphere, we begin with a closed 3-simplex (a tetrahedron).  For 
                                  // each face, connect the midpoints of each edge together to make more faces.  Repeat recursively until 
                                  // the desired level of detail is obtained.  Project all new vertices to unit vectors (onto the unit 
  constructor( max_subdivisions ) // sphere) and group them into triangles by following the predictable pattern of the recursion.
    { super( "positions", "normals", "texture_coords" );
      this.positions.push( ...Vec.cast( [ 0, 0, -1 ], [ 0, .9428, .3333 ], [ -.8165, -.4714, .3333 ], [ .8165, -.4714, .3333 ] ) );  // Start with this equilateral tetrahedron
      
      this.subdivideTriangle( 0, 1, 2, max_subdivisions);  // Begin recursion.
      this.subdivideTriangle( 3, 2, 1, max_subdivisions);
      this.subdivideTriangle( 1, 0, 3, max_subdivisions);
      this.subdivideTriangle( 0, 2, 3, max_subdivisions); 
      
      for( let p of this.positions )
        { this.normals       .push( Vec.of( ...p ) );    // Each point has a normal vector that simply goes to the point from the origin.  Copy array by value.
          this.texture_coords.push( Vec.of( Math.asin( p[0]/Math.PI ) + .5, Math.asin( p[1]/Math.PI ) + .5 ) ) }
    }  // A Subdivision sphere has no seams to which image edges in UV space can be mapped.  You are forced to smoothly wrap & unwrap the image in reverse.
  subdivideTriangle( a, b, c, count )   // Recurse through each level of detail by splitting triangle (a,b,c) into four smaller ones.
    { 
      if( count <= 0) { this.indices.push(a,b,c); return; }  // Base case of recursion - we've hit the finest level of detail we want.
                  
      var ab_vert = this.positions[a].mix( this.positions[b], 0.5).normalized(),     // We're not at the base case.  So,
          ac_vert = this.positions[a].mix( this.positions[c], 0.5).normalized(),     // build 3 new vertices at midpoints, and extrude them out to
          bc_vert = this.positions[b].mix( this.positions[c], 0.5).normalized();     // touch the unit sphere (length 1).
            
      var ab = this.positions.push( ab_vert ) - 1,      // Here, push() returns the indices of the three new vertices (plus one).
          ac = this.positions.push( ac_vert ) - 1,  
          bc = this.positions.push( bc_vert ) - 1;  
      
      this.subdivideTriangle( a, ab, ac,  count - 1 );      // Recurse on four smaller triangles, and we're done.
      this.subdivideTriangle( ab, b, bc,  count - 1 );      // Skipping every fourth vertex index in our list takes you down one level of detail, and 
      this.subdivideTriangle( ac, bc, c,  count - 1 );      // so on, due to the way we're building it.
      this.subdivideTriangle( ab, bc, ac, count - 1 );
    }
}

window.Cube = window.classes.Cube =
class Cube extends Shape    // A cube inserts six square strips into its arrays.
{ constructor()  
    { super( "positions", "normals", "texture_coords" );
      for( var i = 0; i < 3; i++ )                    
        for( var j = 0; j < 2; j++ )
        { var square_transform = Mat4.rotation( i == 0 ? Math.PI/2 : 0, Vec.of(1, 0, 0) ).times( Mat4.rotation( Math.PI * j - ( i == 1 ? Math.PI/2 : 0 ), Vec.of( 0, 1, 0 ) ) )
                                                                                         .times( Mat4.translation([ 0, 0, 1 ]) );
          Square.insert_transformed_copy_into( this, [], square_transform );             
        }
    }
}

window.Line_Segment_Array = window.classes.Line_Segment_Array =
class Line_Segment_Array extends Shape    // Plot 2D points.
{ constructor()
  { super( "positions", "colors" );
    this.indexed = false;
  }
  set_data( origins, destinations, colors, gl = this.gl )      // Provide two lists of points (each pair will be connected into a segment),
    { this.positions = [];                                     // plus a list of enough colors for each of those two points per segment.
      for( let [i] of origins.entries() )
      { this.positions[ 2*i     ] = origins[i];  
        this.positions[ 2*i + 1 ] = destinations[i];
      }
      this.colors = colors;
      this.copy_onto_graphics_card( gl, [ "positions", "colors" ], false );
    }
  execute_shaders( gl ) { gl.drawArrays( gl.LINES, 0, this.positions.length ) }   // Same as normal draw, but with gl.LINES.
}

window.Basic_Shader = window.classes.Basic_Shader =
class Basic_Shader extends Shader         // Simplest example shader.  Sample pixels from colors that are directly assigned to the vertices.
{ material() { return { shader: this } }  // Materials here are minimal, without any settings.
  map_attribute_name_to_buffer_name( name )     // We'll pull single values out per vertex by name.  Map those names onto the arrays we'll pull them from.
    { return { object_space_pos: "positions", color: "colors" }[ name ]; }
  update_GPU( g_state, model_transform, material, gpu = this.g_addrs, gl = this.gl )     // Define how to synchronize our JavaScript's variables to the GPU's:
      { const [ P, C, M ] = [ g_state.projection_transform, g_state.camera_transform, model_transform ],
                      PCM = P.times( C ).times( M );
        gl.uniformMatrix4fv( gpu.projection_camera_model_transform_loc, false, Mat.flatten_2D_to_1D( PCM.transposed() ) );
      }
  shared_glsl_code()            // ********* SHARED CODE INCLUDED IN BOTH SHADERS *********
    { return `precision mediump float;
              varying vec4 VERTEX_COLOR;
      `;
    }
  vertex_glsl_code()           // ********* VERTEX SHADER *********
    { return `
        attribute vec4 color;
        attribute vec3 object_space_pos;
        uniform mat4 projection_camera_model_transform;

        void main()
        { gl_Position = projection_camera_model_transform * vec4(object_space_pos, 1.0);      // The vertex's final resting place onscreen in normalized coords.         
          VERTEX_COLOR = color;
        }`;
    }
  fragment_glsl_code()           // ********* FRAGMENT SHADER *********
    { return `
        void main()
        { gl_FragColor = VERTEX_COLOR;
        }`;
    }
}

window.Funny_Shader = window.classes.Funny_Shader =
class Funny_Shader extends Shader         // Simple "procedural" texture shader, with texture coordinates but without an input image.
{ material() { return { shader: this } }  // Materials here are minimal, without any settings.
  map_attribute_name_to_buffer_name( name )     // We'll pull single values out per vertex by name.  Map those names onto the arrays we'll pull them from.
    { return { object_space_pos: "positions", tex_coord: "texture_coords" }[ name ]; }
  update_GPU( g_state, model_transform, material, gpu = this.g_addrs, gl = this.gl )     // Define how to synchronize our JavaScript's variables to the GPU's:
      { const [ P, C, M ] = [ g_state.projection_transform, g_state.camera_transform, model_transform ],
                      PCM = P.times( C ).times( M );
        gl.uniformMatrix4fv( gpu.projection_camera_model_transform_loc, false, Mat.flatten_2D_to_1D( PCM.transposed() ) );
        gl.uniform1f ( gpu.animation_time_loc, g_state.animation_time / 1000 );
      }
  shared_glsl_code()            // ********* SHARED CODE INCLUDED IN BOTH SHADERS *********
    { return `precision mediump float;
              varying vec2 f_tex_coord;
      `;
    }
  vertex_glsl_code()           // ********* VERTEX SHADER *********
    { return `
        attribute vec3 object_space_pos;
        attribute vec2 tex_coord;
        uniform mat4 projection_camera_model_transform;

        void main()
        { gl_Position = projection_camera_model_transform * vec4(object_space_pos, 1.0);      // The vertex's final resting place onscreen in normalized coords.         
          f_tex_coord = tex_coord;                                                            // Directly use original texture coords and interpolate between.
        }`;
    }
  fragment_glsl_code()           // ********* FRAGMENT SHADER *********
    { return `
        uniform float animation_time;
        void main()
        { float a = animation_time, u = f_tex_coord.x, v = f_tex_coord.y;

          gl_FragColor = vec4(
            2.0 * u * sin(17.0 * u ) + 3.0 * v * sin(11.0 * v ) + 1.0 * sin(13.0 * a),
            3.0 * u * sin(18.0 * u ) + 4.0 * v * sin(12.0 * v ) + 2.0 * sin(14.0 * a),
            4.0 * u * sin(19.0 * u ) + 5.0 * v * sin(13.0 * v ) + 3.0 * sin(15.0 * a),
            5.0 * u * sin(20.0 * u ) + 6.0 * v * sin(14.0 * v ) + 4.0 * sin(16.0 * a));
        }`;
    }
}

window.Phong_Model = window.classes.Phong_Model =
class Phong_Model extends Shader          // THE DEFAULT SHADER: Phong Reflection Model (with Gouraud option) (also see Wikipedia article)
// Subclasses of Shader each store and manage a complete GPU program.
// The "vertex_glsl_code" string below is code that is sent to the graphics card at runtime, where on each run it gets compiled and linked there.  Thereafter, all of your 
// calls to draw shapes will launch the vertex shader program once per vertex in the shape (three times per triangle), sending results on to the next phase.  The purpose
// of this vertex shader program is to calculate the final resting place of vertices in screen coordinates; each of them starts out in local object coordinates.

// Likewise, the "fragment_glsl_code" string is used as the Fragment Shader program, which gets sent to the graphics card at runtime.  The fragment shader runs once all 
// the vertices in a triangle / element finish their vertex shader programs, and thus have finished finding out where they land on the screen.  The fragment shader fills
// in (shades) every pixel (fragment) overlapping where the triangle landed.  At each pixel it interpolates different values from the three extreme points of the triangle, 
// and uses them in formulas to determine color.  The fragment colors may or may not become final pixel colors; there could already be other triangles' fragments occupying 
// the same pixels.  The Z-Buffer test is applied to see if the new triangle is closer to the camera, and even if so, blending settings may interpolate some of the old color 
// into the result.
{ material( color, properties )     // Define an internal class "Material" that stores the standard settings found in a Phong lighting model.
  { return new class Material       // Possible properties: ambient, diffusivity, specularity, smoothness, texture.
      { constructor( shader, color = Color.of( 0,0,0,1 ), ambient = 0, diffusivity = 1, specularity = 1, smoothness = 40 )
          { Object.assign( this, { shader, color, ambient, diffusivity, specularity, smoothness } );    // Assign defaults.
            Object.assign( this, properties );                                                        // Optionally override defaults.
          }
        override( properties )                      // Easily make temporary overridden versions of a base material, such as
          { const copied = new this.constructor();  // of a different color or diffusivity.  Use "opacity" to override only that.
            Object.assign( copied, this );
            Object.assign( copied, properties );
            copied.color = copied.color.copy();
            if( properties[ "opacity" ] != undefined ) copied.color[3] = properties[ "opacity" ];
            return copied;
          }
      }( this, color );
  }
  map_attribute_name_to_buffer_name( name )     // We'll pull single values out per vertex by name.  Map those names onto the arrays we'll pull them from.
    { return { object_space_pos: "positions", normal: "normals", tex_coord: "texture_coords" }[ name ]; }
  shared_glsl_code()            // ********* SHARED CODE INCLUDED IN BOTH SHADERS *********
    { return `precision mediump float;
        const int N_LIGHTS = 2;                                                         // Be sure to keep this line up to date as you add more lights
        uniform float ambient, diffusivity, specularity, smoothness, animation_time, attenuation_factor[N_LIGHTS];
        uniform bool GOURAUD, COLOR_NORMALS, USE_TEXTURE;               // Flags for alternate shading methods
        uniform vec4 lightPosition[N_LIGHTS], lightColor[N_LIGHTS], shapeColor;
        varying vec3 N, E;            // Specifier "varying" means it will be passed from the vertex shader on to the fragment shader, 
        varying vec2 f_tex_coord;     // then interpolated per-fragment, weighted by the pixel fragment's proximity to each of the 3 vertices.          
        varying vec4 VERTEX_COLOR;
        varying vec3 L[N_LIGHTS], H[N_LIGHTS];
        varying float dist[N_LIGHTS];
        
        vec3 phong_model_lights( vec3 N )
          { vec3 result = vec3(0.0);
            for(int i = 0; i < N_LIGHTS; i++)
              {
                float attenuation_multiplier = 1.0 / (1.0 + attenuation_factor[i] * (dist[i] * dist[i]));
                float diffuse  =      max( dot(N, L[i]), 0.0 );
                float specular = pow( max( dot(N, H[i]), 0.0 ), smoothness );

                result += attenuation_multiplier * ( shapeColor.xyz * diffusivity * diffuse + lightColor[i].xyz * specularity * specular );
                result  = clamp( result, 0.0, 1.0 );
              }
            return result;
          }
        `;
    }
  vertex_glsl_code()           // ********* VERTEX SHADER *********
    { return `
        attribute vec3 object_space_pos, normal;
        attribute vec2 tex_coord;

        uniform mat4 camera_transform, camera_model_transform, projection_camera_model_transform;
        uniform mat3 inverse_transpose_modelview;

        void main()
        { gl_Position = projection_camera_model_transform * vec4(object_space_pos, 1.0);      // The vertex's final resting place onscreen in normalized coords.
          N = normalize( inverse_transpose_modelview * normal );                              // The final normal vector in screen space.
          f_tex_coord = tex_coord;                                                            // Directly use original texture coords and interpolate between.
          
          if( COLOR_NORMALS )                                               // Bypass all lighting code if we're lighting up vertices some other way.
          { VERTEX_COLOR = vec4( N[0] > 0.0 ? N[0] : sin( animation_time * 3.0   ) * -N[0],             // In normals mode, rgb color = xyz quantity.  
                                 N[1] > 0.0 ? N[1] : sin( animation_time * 15.0  ) * -N[1],             // Flash if it's negative.
                                 N[2] > 0.0 ? N[2] : sin( animation_time * 45.0  ) * -N[2] , 1.0 );
            return;
          }
                                                                                // The rest of this shader calculates some quantities that the Fragment shader will need:
          vec3 screen_space_pos = ( camera_model_transform * vec4(object_space_pos, 1.0) ).xyz;
          E = normalize( -screen_space_pos );

          for( int i = 0; i < N_LIGHTS; i++ )
          {
            L[i] = normalize( ( camera_transform * lightPosition[i] ).xyz - lightPosition[i].w * screen_space_pos );   // Use w = 0 for a directional light source -- a 
            H[i] = normalize( L[i] + E );                                                                              // vector instead of a point.
                                                    // Is it a point light source?  Calculate the distance to it from the object.  Otherwise use some arbitrary distance.
            dist[i]  = lightPosition[i].w > 0.0 ? distance((camera_transform * lightPosition[i]).xyz, screen_space_pos)
                                                : distance( attenuation_factor[i] * -lightPosition[i].xyz, object_space_pos.xyz );
          }

          if( GOURAUD )         // Gouraud shading mode?  If so, finalize the whole color calculation here in the vertex shader, one per vertex, before we even 
          {                     // break it down to pixels in the fragment shader.   As opposed to Smooth "Phong" Shading, where we do calculate it afterwards.
            VERTEX_COLOR      = vec4( shapeColor.xyz * ambient, shapeColor.w);
            VERTEX_COLOR.xyz += phong_model_lights( N );
          }
        }`;
    }                            // A fragment is a pixel that's overlapped by the current triangle.  Fragments affect the final image or get discarded due to depth.
  fragment_glsl_code()           // ********* FRAGMENT SHADER ********* 
    { return `
        uniform sampler2D texture;
        void main()
        { if( GOURAUD || COLOR_NORMALS )    // Do smooth "Phong" shading unless these options say not to.
          { gl_FragColor = VERTEX_COLOR;    // In the Gouraud case, we already have final colors to smear (interpolate) across vertices.            
            return;
          }                                 // Calculate Smooth "Phong" Shading (not to be confused with the Phong Reflection Model).  As opposed to Gouraud Shading.
          vec4 tex_color = texture2D( texture, f_tex_coord );                         // Use texturing as well
 
          if( USE_TEXTURE ) gl_FragColor = vec4( ( tex_color.xyz + shapeColor.xyz ) * ambient, shapeColor.w * tex_color.w ); 
          else gl_FragColor = vec4( shapeColor.xyz * ambient, shapeColor.w );
          gl_FragColor.xyz += phong_model_lights( N );
        }`;
    }
  update_GPU( g_state, model_transform, material, gpu = this.g_addrs, gl = this.gl )     // Define how to synchronize our JavaScript's variables to the GPU's:
    { 
      this.update_matrices( g_state, model_transform, gpu, gl );    // (Send the matrices, additionally cache-ing some products of them we know we'll need.)
      gl.uniform1f ( gpu.animation_time_loc, g_state.animation_time / 1000 );

      if( g_state.gouraud === undefined ) { g_state.gouraud = g_state.color_normals = false; }    // (Keep the flags seen by the shader program
      gl.uniform1i( gpu.GOURAUD_loc,        g_state.gouraud       );                              //  up-to-date and make sure they are declared.)
      gl.uniform1i( gpu.COLOR_NORMALS_loc,  g_state.color_normals );

      gl.uniform4fv( gpu.shapeColor_loc,     material.color       );    // (Send the desired shape-wide material qualities to the graphics card)
      gl.uniform1f ( gpu.ambient_loc,        material.ambient     ); 
      gl.uniform1f ( gpu.diffusivity_loc,    material.diffusivity );
      gl.uniform1f ( gpu.specularity_loc,    material.specularity );
      gl.uniform1f ( gpu.smoothness_loc,     material.smoothness  );

      if( material.texture )  // (Omit the texture parameter to signal not to draw a texture.)
      { gpu.shader_attributes["tex_coord"].enabled = true;
        gl.uniform1f ( gpu.USE_TEXTURE_loc, 1 );
        gl.bindTexture( gl.TEXTURE_2D, material.texture.id );
      }
      else  { gl.uniform1f ( gpu.USE_TEXTURE_loc, 0 );   gpu.shader_attributes["tex_coord"].enabled = false; }

      if( !g_state.lights.length )  return;
      var lightPositions_flattened = [], lightColors_flattened = [], lightAttenuations_flattened = [];
      for( var i = 0; i < 4 * g_state.lights.length; i++ )
        { lightPositions_flattened                  .push( g_state.lights[ Math.floor(i/4) ].position[i%4] );
          lightColors_flattened                     .push( g_state.lights[ Math.floor(i/4) ].color[i%4] );
          lightAttenuations_flattened[ Math.floor(i/4) ] = g_state.lights[ Math.floor(i/4) ].attenuation;
        }
      gl.uniform4fv( gpu.lightPosition_loc,       lightPositions_flattened );
      gl.uniform4fv( gpu.lightColor_loc,          lightColors_flattened );
      gl.uniform1fv( gpu.attenuation_factor_loc,  lightAttenuations_flattened );
    }
  update_matrices( g_state, model_transform, gpu, gl )                                                  // Helper function for sending matrices to GPU
    { let [ P, C, M ]    = [ g_state.projection_transform, g_state.camera_transform, model_transform ],   // (PCM will mean Projection * Camera * Model)
            CM     =      C.times(  M ),
            PCM    =      P.times( CM ),                           // Send the current matrices to the shader.  Go ahead and pre-compute the products we'll 
            inv_CM = Mat4.inverse( CM ).sub_block([0,0], [3,3]);   // need of the of the three special matrices and just cache and send those.  They will be 
                                                                   // the same throughout this draw call & thus across each instance of the vertex shader.
      gl.uniformMatrix4fv( gpu.camera_transform_loc,                  false, Mat.flatten_2D_to_1D(     C .transposed() ) );    // GPU expects matrices as column-major arrays.
      gl.uniformMatrix4fv( gpu.camera_model_transform_loc,            false, Mat.flatten_2D_to_1D(     CM.transposed() ) );
      gl.uniformMatrix4fv( gpu.projection_camera_model_transform_loc, false, Mat.flatten_2D_to_1D(    PCM.transposed() ) );
      gl.uniformMatrix3fv( gpu.inverse_transpose_modelview_loc,       false, Mat.flatten_2D_to_1D( inv_CM              ) );       
    }
}

window.Fake_Bump_Map = window.classes.Fake_Bump_Map =
class Fake_Bump_Map extends Phong_Model  // Overrides Phong_Model except for one thing                  
{ fragment_glsl_code()           // ********* FRAGMENT SHADER *********
    { return `
        uniform sampler2D texture;          //  Like real bump mapping, but with no separate file for the bump map (instead we'll
        void main()                         //  re-use the colors of the original picture file to disturb the normal vectors)
        { if( GOURAUD || COLOR_NORMALS )    // Do smooth "Phong" shading unless these options say not to.
          { gl_FragColor = VERTEX_COLOR;    // In the Gouraud case, we already have final colors to smear (interpolate) across vertices.            
            return;
          }                                 // Calculate Smooth "Phong" Shading (not to be confused with the Phong Reflection Model).  As opposed to Gouraud Shading.
          vec4 tex_color = texture2D( texture, f_tex_coord );                         // Use texturing as well
          vec3 bumped_N  = normalize( N + tex_color.rgb - .5*vec3(1,1,1) );           // Slightly disturb normals based on sampling the same texture
          
          if( USE_TEXTURE ) gl_FragColor = vec4( ( tex_color.xyz + shapeColor.xyz ) * ambient, shapeColor.w * tex_color.w ); 
          else gl_FragColor = vec4( shapeColor.xyz * ambient, shapeColor.w );
          gl_FragColor.xyz += phong_model_lights( bumped_N );
        }`;
    }
}



window.Common_Materials_Shapes = window.classes.Common_Materials_Shapes =
class Common_Materials_Shapes                                             // Warning:  Do not instantiate this class often; only once is enough.
  { constructor( context, include_textures = true, include_closed = true, include_revolution = false, include_patches = false )
      { this.shapes = {}
        if( include_textures )   Object.assign( this, this.common_textures( context ) )
        if( include_closed )     Object.assign( this.shapes, this.closed_shapes() )      
        if( include_revolution ) Object.assign( this.shapes, this.revolution_surfaces() )       
        if( include_patches )    Object.assign( this.shapes, this.custom_patches() )   
      }
    common_textures( context )
      { return { rgb   : context.get_instance( "/assets/rgb.jpg"   ),
                 earth : context.get_instance( "/assets/earth.gif" ),
                 grid  : context.get_instance( "/assets/grid.png"  ),
                 stars : context.get_instance( "/assets/stars.png" ),
                 text  : context.get_instance( "/assets/text.png"  )
               }
      }      
    closed_shapes()
      { return { donut  : new Torus          ( 15, 15 ),
                 cone   : new Closed_Cone    ( 4, 10 ),
                 capped : new Capped_Cylinder( 4, 12 ),
                 ball   : new Subdivision_Sphere( 3 ),
                 cube   : new Cube(),
                 axis   : new Axis_Arrows(),
                 prism  : new ( Capped_Cylinder   .prototype.make_flat_shaded_version() )( 10, 10 ),
                 gem    : new ( Subdivision_Sphere.prototype.make_flat_shaded_version() )( 2 ),
                 donut  : new ( Torus             .prototype.make_flat_shaded_version() )( 20, 20 ) 
               }; 
      }
    revolution_surfaces()
      { return { tube        : new Cylindrical_Tube  ( 10, 10, [[0,1],[0,1]] ),
                 open_cone   : new Cone_Tip          (  3, 10, [[0,1],[0,1]] ),
                 gem2        : new ( Torus.prototype.make_flat_shaded_version() )( 20, 20 ),
                 bad_sphere  : new Grid_Sphere       ( 10, 10 ),                           // A sphere made of rows and columns, with singularities
                 septagon    : new Regular_2D_Polygon(  2,  7 ),                 
                 swept_curve : new Surface_Of_Revolution( 10, 10, Vec.cast( [2, 0, -1], [1, 0, 0], [1, 0, 1], [0, 0, 2] ), [ [ 0, 1 ], [ 0, 7 ] ], Math.PI/3 ),        
               };
      }
    custom_patches()
      { let square_array = Vec.cast( [ 1,0,-1 ], [ 0,1,-1 ], [ -1,0,-1 ], [ 0,-1,-1 ], [ 1,0,-1 ] ),               // Some helper arrays of points located along
            star_array = Array(19).fill( Vec.of( 1,0,-1 ) ), circle_array = Array(40).fill( Vec.of( 1,0,-1 ) );  // curves.  We'll extrude these into surfaces.
            circle_array = circle_array.map( (x,i,a) => Mat4.rotation( i/(a.length-1) * 2*Math.PI, Vec.of( 0,0,1 ) ).times( x.to4(1) ).to3() );
            star_array   =   star_array.map( (x,i,a) => Mat4.rotation( i/(a.length-1) * 2*Math.PI, Vec.of( 0,0,1 ) ).times( Mat4.translation([ (i%2)/2,0,0 ]) ).times( x.to4(1) ).to3() );

        let sin_rows_func       =      i  => { return Vec.of( .5 + Math.sin(777*i)/4, 2-4*i, 0 ) },                                   // Different callbacks for telling Grid_Patch 
          sin_columns_func    = ( j,p ) => { return Mat4.translation([ Math.sin(777*j)/4,0,4/30    ]).times( p.to4(1) ).to3() },    // how it chould advance to the next row/column.  
          rotate_columns_func = ( j,p ) => { return Mat4.rotation( .1*j*Math.PI, Vec.of( 0,1,0 )    ).times( p.to4(1) ).to3() },
          sample_square_func  =      i  => { return Grid_Patch.sample_array( square_array, i ) },
          sample_star_func    =      i  => { return Grid_Patch.sample_array( star_array,   i ) },
          sample_circle_func  =      i  => { return Grid_Patch.sample_array( circle_array, i ) },          
          sample_two_arrays   = (j,p,i) => { return Mat4.translation([0,0,2*j]).times( sample_star_func(i).mix( sample_circle_func(i), j ).to4(1) ).to3() },
          sample_two_arrays2  = (j,p,i) => { return Mat4.rotation( .5*j*Math.PI, Vec.of( 1,1,1 ) ).times( 
                                                    Mat4.translation([0,0,2*j]).times( sample_star_func(i).mix( sample_square_func(i), j ).to4(1) ) ).to3() },
          line_rows_func      = ( i,p ) => { return p ? p.plus( Vec.of( 0,-.02,-.1 ) ) : Vec.of( 0,.2,-2 ) },
          transform_cols_func = (j,p,i) => { return Mat4.rotation( Math.PI/6, Vec.of( 0,0,1 ) ).times( Mat4.scale([ 1.05,1.05,1.01 ])).times( Mat4.translation([ 0,0,.1 ]))
                                                      .times( p.to4(1) ).to3() };
        return { vase   : new Grid_Patch( 30, 30, sin_rows_func, rotate_columns_func,   [[0,1],[0,1]] ),
                 ghost  : new Grid_Patch( 36, 10, sample_star_func, sample_two_arrays,  [[0,1],[0,1]] ),
                 shell  : new Grid_Patch( 10, 40, line_rows_func, transform_cols_func,  [[0,5],[0,1]] ),
                 waves  : new Grid_Patch( 30, 30, sin_rows_func, sin_columns_func,      [[0,1],[0,1]] ),
                 shell2 : new Grid_Patch( 30, 30, sample_star_func, sample_two_arrays2, [[0,1],[0,1]] )
               };
      }
    random_shape( shape_list = this.shapes )
      { const shape_names = Object.keys( shape_list );
        return shape_list[ shape_names[ ~~( shape_names.length * Math.random() ) ] ]
      }
  }

window.Movement_Controls = window.classes.Movement_Controls =
class Movement_Controls extends Scene_Component    // A Scene_Component that our Canvas_Manager can manage.  Adds both first-person and third-person style camera matrix controls to the canvas.
{ constructor( context, canvas = context.canvas )
    { super( context );                            // Initialize some data members:
      Object.assign( this, { context, thrust: Vec.of(0,0,0), roll: 0, look_around_locked: true, pos: Vec.of( 0,0,0 ), z_axis: Vec.of( 0,0,0 ) } );
      this.target = function() { return context.globals.movement_controls_target() }                                      // The camera matrix is not actually stored here inside Movement_Controls; instead, track
      context.globals.movement_controls_target = function(t) { return context.globals.graphics_state.camera_transform };  // an external matrix to modify. This target is a reference (made with closures) kept
      context.globals.has_controls = true;                                                                                // in "globals" so it can be seen and set by other classes.  Initially, the default target
                                                                                                                          // is a camera matrix stored in the global graphics_state object, for Shaders to use.      
      [ this.radians_per_frame, this.meters_per_frame, this.speed_multiplier ] = [ 1/100, 20, 1 ];
      
      // *** Mouse controls: ***
      this.mouse = { "from_center": Vec.of( 0,0 ) };                           // Measure mouse steering, for rotating the flyaround camera:
      const mouse_position = ( e, rect = canvas.getBoundingClientRect() ) => Vec.of( e.clientX - (rect.left + rect.right)/2, e.clientY - (rect.bottom + rect.top)/2 );
      document.addEventListener( "mouseup", e => { this.mouse.anchor = undefined; } );
      canvas.addEventListener( "mousedown", e => { e.preventDefault(); this.mouse.anchor      = mouse_position(e); } );
      canvas.addEventListener( "mousemove", e => { e.preventDefault(); this.mouse.from_center = mouse_position(e); } );
      canvas.addEventListener( "mouseout",  e => { if( !this.mouse.anchor ) this.mouse.from_center.scale(0) } );  // Stop reacting if the mouse leaves the canvas.
    }
  make_control_panel()   // This function of a scene sets up its keyboard shortcuts.
    { const globals = this.globals;
      this.control_panel.innerHTML += "Click and drag the scene to <br> spin your viewpoint around it.<br>";
      this.key_triggered_button( "Up",     [ " " ], function() { this.thrust[1] = -1 }, undefined, function() { this.thrust[1] = 0 } );
      this.key_triggered_button( "Forward",[ "w" ], function() { this.thrust[2] =  1 }, undefined, function() { this.thrust[2] = 0 } ); this.new_line();
      this.key_triggered_button( "Left",   [ "a" ], function() { this.thrust[0] =  1 }, undefined, function() { this.thrust[0] = 0 } );
      this.key_triggered_button( "Back",   [ "s" ], function() { this.thrust[2] = -1 }, undefined, function() { this.thrust[2] = 0 } );
      this.key_triggered_button( "Right",  [ "d" ], function() { this.thrust[0] = -1 }, undefined, function() { this.thrust[0] = 0 } ); this.new_line();
      this.key_triggered_button( "Down",   [ "z" ], function() { this.thrust[1] =  1 }, undefined, function() { this.thrust[1] = 0 } ); 

      const speed_controls = this.control_panel.appendChild( document.createElement( "span" ) );
      speed_controls.style.margin = "30px";
      this.key_triggered_button( "-",   [ "o" ], function() { this.speed_multiplier  /=  1.2 },    "green", undefined, undefined, speed_controls );
      this.live_string( box => { box.textContent = "Speed: " + this.speed_multiplier.toFixed(2) }, speed_controls );
      this.key_triggered_button( "+",   [ "p" ], function() { this.speed_multiplier  *=  1.2 },    "green", undefined, undefined, speed_controls );
      this.new_line();
      this.key_triggered_button( "Roll left",  [ "," ], function() { this.roll      =  1 }, undefined, function() { this.roll      = 0 } );
      this.key_triggered_button( "Roll right", [ "." ], function() { this.roll      = -1 }, undefined, function() { this.roll      = 0 } ); this.new_line();
      this.key_triggered_button( "(Un)freeze mouse look around", [ "f" ], function() { this.look_around_locked  ^=  1 },    "green" );    this.new_line();
      this.live_string( box => { box.textContent = "Position: "            + this.   pos[0].toFixed(2) + ", " + this.   pos[1].toFixed(2) + ", " + this.   pos[2].toFixed(2) } ); this.new_line();
      this.live_string( box => { box.textContent = "Facing: " + ( ( this.z_axis[0] > 0 ? "West " : "East ")             // (Actually affected by the left hand rule)
                                                    + ( this.z_axis[1] > 0 ? "Down " : "Up " ) + ( this.z_axis[2] > 0 ? "North" : "South" ) ) } ); this.new_line();
      
      this.key_triggered_button( "Go to world origin", [ "r" ], function() { this.target().set_identity( 4,4 ) }, "orange" ); this.new_line();
      this.key_triggered_button( "Reset target to main camera", [ "Shift", "R" ], function() { globals.movement_controls_target = function() { return globals.graphics_state.camera_transform }; }, "blue" ); this.new_line();
    }  
  first_person_flyaround( radians_per_frame, meters_per_frame, leeway = 70 )  // Determine camera rotation movement when the mouse is past a minimum distance (leeway) from the canvas's center.
    { const offsets_from_dead_box = { plus: [ this.mouse.from_center[0] + leeway, this.mouse.from_center[1] + leeway ],
                                     minus: [ this.mouse.from_center[0] - leeway, this.mouse.from_center[1] - leeway ] };    // Compare mouse's location to all four corners of dead box.
      if( !this.look_around_locked ) 
        for( let i = 0; i < 2; i++ )      // Steer according to "mouse_from_center" vector, but don't start increasing until outside a leeway window from the center.
        { let o = offsets_from_dead_box, velocity = ( ( o.minus[i] > 0 && o.minus[i] ) || ( o.plus[i] < 0 && o.plus[i] ) ) * radians_per_frame;  // The &&'s can zero these out.
          this.target().pre_multiply( Mat4.rotation( +velocity, Vec.of( i, 1-i, 0 ) ) );   // On X step, rotate around Y axis, and vice versa.
        }
      if( this.roll != 0 ) this.target().pre_multiply( Mat4.rotation( +.1, Vec.of(0, 0, this.roll ) ) );
      this.target().pre_multiply( Mat4.translation( this.thrust.times( +meters_per_frame ) ) ); // Now apply translation movement of the camera, in the newest local coordinate frame
    }
  third_person_arcball( radians_per_frame )
    { const dragging_vector = this.mouse.from_center.minus( this.mouse.anchor );  // Spin the scene around a point on a user-determined axis.
      if( dragging_vector.norm() > 0 )
        this.target().pre_multiply( Mat4.translation([ 0,0, 25 ]) )
                     .pre_multiply( Mat4.rotation( radians_per_frame * dragging_vector.norm(), Vec.of( dragging_vector[1], dragging_vector[0], 0 ) ) )
                     .pre_multiply( Mat4.translation([ 0,0,-25 ]) )
    }
  display( graphics_state, dt = graphics_state.animation_delta_time / 1000 )    // Camera code starts here.
    { const m = this.speed_multiplier * this.meters_per_frame, r = this.speed_multiplier * this.radians_per_frame;
      this.first_person_flyaround( dt * r, dt * m );     // Scale the normal camera aiming speed by dt for smoothness.
      if( this.mouse.anchor ) this.third_person_arcball( dt * r);           // Also apply third-person "arcball" camera mode if a mouse drag is occurring.  
      
      const inv = Mat4.inverse( this.target() ); this.pos = inv.times( Vec.of( 0,0,0,1 ) ); this.z_axis = inv.times( Vec.of( 0,0,1,0 ) );      // Log some values.
    }
}


window.Global_Info_Table = window.classes.Global_Info_Table =
class Global_Info_Table extends Scene_Component  // A class that just toggles, monitors, and reports some global values via its control panel.
{ make_control_panel()
    { const globals = this.globals;
      globals.has_info_table = true;
      this.key_triggered_button( "(Un)pause animation", ["Alt", "a"], function() { globals.animate ^= 1; } ); this.new_line();
      this.live_string( box => { box.textContent = "Animation Time: " + ( globals.graphics_state.animation_time/1000 ).toFixed(3) + "s" } );
      this.live_string( box => { box.textContent = globals.animate ? " " : " (paused)" } );  this.new_line();
      this.key_triggered_button( "Gouraud shading",     ["Alt", "g"], function() { globals.graphics_state.gouraud       ^= 1;         } ); this.new_line();
      this.key_triggered_button( "Normals shading",     ["Alt", "n"], function() { globals.graphics_state.color_normals ^= 1;         } ); this.new_line();
      
      const label = this.control_panel.appendChild( document.createElement( "p" ) );
      label.style = "align:center";
      label.innerHTML = "A shared scratchpad is <br> accessible to all Scene_Components. <br> Navigate its values here:";

      const show_object = ( element, obj = globals ) => 
      { //element.innerHTML = "<p align=center>Any values placed in this <br> scratchpad can be accessed <br> by all Scene_Components:</p>";
        if( this.box ) this.box.innerHTML = "";
        else this.box = element.appendChild( Object.assign( document.createElement( "div" ), { style: "overflow:auto; width: 200px" } ) );
        if( obj !== globals )
          this.box.appendChild( Object.assign( document.createElement( "div" ), { className:"link", innerText: "(back to globals)", onmousedown: () => this.current_object = globals } ) )
        if( obj.to_string ) return this.box.appendChild( Object.assign( document.createElement( "div" ), { innerText: obj.to_string() } ) );
        for( let [key,val] of Object.entries( obj ) )
        { if( typeof( val ) == "object" ) 
            this.box.appendChild( Object.assign( document.createElement( "a" ), { className:"link", innerText: key, onmousedown: () => this.current_object = val } ) )
          else
            this.box.appendChild( Object.assign( document.createElement( "span" ), { innerText: key + ": " + val.toString() } ) );
          this.box.appendChild( document.createElement( "br" ) );
        }
      }
      this.live_string( box => show_object( box, this.current_object ) );      
    }
}