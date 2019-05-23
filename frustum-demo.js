window.Frustum_Tool = window.classes.Frustum_Tool =
class Frustum_Tool extends Scene_Component
{ constructor( context, graphics_state )
    { super( context );
      const outline = new Line_Segment_Array();
      this.instance_name = "outline"+Math.random();
      this.submit_shapes( context, { [this.instance_name]: outline } );
      this.drawn_graphics_state = graphics_state;
      this.aspect_ratio = context.width / context.height;
      
      Object.assign( this, { width: context.width, height: context.height } );
      this.vertex_color = context.get_instance( Basic_Shader );      
    }
  static derive_frustum_points_from_matrix( m, points )
    { return points.map( p => Mat4.inverse( m ).times( p.to4(1) ) )    // Apply the linear projection matrix to the points cube.
                   .map( p => p.map( x => x/p[3] ).to3() );   // Manually do a perspective division of all coordinates by the fourth coordinate.   
      
      // That's all we needed; at this point we have the final points of the frutsum box and are ready to draw the box.      
      // You might expect a multiply to undo the division to be performed instead, before the matrix; due to complex math, doing it this way instead
      // not only reverses the division in a sign-correct way but correctly sets the Z coordinate.  We otherwise wouldn't know which Z to undivide by ahead of time and the 
      // projection wipes it out in a non-invertible way.  This works irrespective of perspective or orthographic or parameters.
    }
  make_control_panel() 
    { this.key_triggered_button( "Assign Perspective",  [ "Alt","p" ], function() { 
          this.drawn_graphics_state.projection_transform = Mat4.perspective( Math.PI/4, this.aspect_ratio, 10, 25 ) }, "Red" ); this.new_line();
      this.key_triggered_button( "Assign Orthographic", [ "Alt","o" ], function() { 
          this.drawn_graphics_state.projection_transform = Mat4.orthographic( -10*this.aspect_ratio, 10*this.aspect_ratio, -10, 10, 10, 25 ) }, "Blue" );
    }
  display( graphics_state, brightness = 1 )
    { if( !this.drawn_graphics_state )  // If our graphics_state is empty, copy the one provided the first frame.
           this.drawn_graphics_state = Object.assign( new Graphics_State(), graphics_state, 
                                                        { camera_transform: graphics_state.camera_transform.copy(), 
                                                      projection_transform: graphics_state.projection_transform.copy() } );   
     
      const view_box_normalized = Vec.cast( [-1, -1, -1],  [1,  -1, -1],  [-1, 1, -1],  [1,  1, -1],  [-1, -1, 1],  [1,  -1, 1],  [-1, 1, 1],  [1,  1, 1] );
      const points = Frustum_Demo.derive_frustum_points_from_matrix( this.drawn_graphics_state.projection_transform, view_box_normalized );

      // Draw lines between the correct points to draw the edges of the frustum box.
      let origins      = [ points[6], points[7], points[5], points[4], points[2], points[3], points[1], points[0], points[2], points[3], points[1], points[0] ];
      let destinations = [ points[7], points[5], points[4], points[6], points[3], points[1], points[0], points[2], points[6], points[7], points[5], points[4] ];      
      origins.push( ...Array( 4 ).fill( 0 ).map( x => Vec.of( 0,0,0 ) ) );
      destinations.push( points[0], points[1], points[2], points[3] );
           
      let colors = [ ...Array( 24 ).fill( 0 ).map( x => Color.of( 0,1,0,brightness ) ), 
                     ...Array(  8 ).fill( 0 ).map( x => Color.of( 1,0,0,brightness ) ) ];
      this.shapes[ this.instance_name ].set_data( origins, destinations, colors );

      this.shapes[ this.instance_name ].draw( graphics_state, Mat4.inverse( this.drawn_graphics_state.camera_transform ), this.vertex_color.material() );
    }
}

window.Frustum_Demo = window.classes.Frustum_Demo =
class Frustum_Demo extends Frustum_Tool
{ constructor( context )
    { super( context );
      if( !context.globals.has_controls   ) context.register_scene_component( new Movement_Controls( context ) ); // This scene includes a couple other 
      if( !context.globals.has_info_table ) context.register_scene_component( new Global_Info_Table( context ) ); // often-helpful scenes.      
      
      this.drawn_graphics_state = new Graphics_State();
      this.drawn_graphics_state.projection_transform = Mat4.perspective( Math.PI/4, context.width/context.height, 10, 25 );

      const ball = new Grid_Sphere( 7, 13 );
      this.submit_shapes( context, { ball } );
      
      this.plastic = context.get_instance( Phong_Model ).material( Color.of( 1,0,1,1 ), { ambient: .2 } );
      
      this.ball_positions = Array( 40 ).fill( 0 ).map( x => Vec.of( 0,0,-10 ).randomized( 30 ).mult_pairs([ 1,1,2 ]) ).filter( p => p.norm() > 2.5 );
      this.balls = this.ball_positions.map( p => ({ position: p, color: Color.of( .5,.5,.5,.5 ).randomized( 1 ).normalized() }) );
      
      context.globals.graphics_state.lights = [ new Light( Vec.of( 1,1,0,0 ).normalized(), Color.of(  1, .5, .5, 1 ), 10000 ),
                                                new Light( Vec.of( 0,1,0,0 ).normalized(), Color.of( .5,  1, .5, 1 ), 10000 ) ];
    }
  show_explanation( document_element )
    { document_element.innerHTML += `<p>This article demonstrates what projection matrices are by displaying a frustum shape (a pyramid pointed at you with its nose cut off) corresponding to the current projection matrix.  We draw it out of lines at the original camera position.</p>
                                     <p>To draw the current frustum this demo doesn't need to know what type of projection (orthographic or perspective) was used to make that matrix -- either one produces a linear 4x4 matrix.  That matrix maps the eight corners of one box onto another.  The first box is a frustum shape in front of the camera; the second is a cube from -1 to 1 on all axes.  Any other points in the scene will be placed inside the latter cube by this transform only if they started off inside the original frustum.  Any points that fall outside the cube will not make it into the final drawing; they will be clipped out of frame after being projected (flattened) onto the near plane and falling outside the -1 to 1 square along x and y.</p>
                                     <p>To draw the current frustum we just need to retreive its corner points, by applying the inverse of the projection matrix to the cube with corner points from -1 to 1 on all axes.</p>
                                     <p>Projection matrices have a special fourth row that sets the fourth coordinate to either 1 (if orthographic) or something else to divide all other coordinates by later (if perspective).  Here we have to do this perspective division manually, whereas during clipping on a graphics card it is automatic.</p>
                                     <p>Instructions:  Use the movement controls to back away from this scene; pieces will go dark, because they have left the view frustum.  Normally these pieces would not be drawn at all, but a second pass with a larger frustum is used to draw them dark.</p>
                                     <p>Back away until the whole scene is dark.  An outline of a frustum is drawn; notice which balls fall inside of it.  Press r to reset the camera back to its starting point.  From there, our real view frustum will match the one drawn with lines, and balls that fall inisde the latter will match those that have been brightened.  Balls that fall behind the far plane or in front of the near plane will appear dark, and all others outside the frustum will be clipped out (not drawn at all).</p>
                                     <p>This scene can be used as a tool used for measuring other scenes.  Remember that you can show multiple demos at once by including both in the URL separated with ampersands (<a href=/Frustum_Demo&Tutorial_Animation target=blank>example</a>).  This frustum drawing tool can help diagnose the projection matrix those other scenes use.</p>`;
    }
  make_control_panel() 
    { super.make_control_panel();  this.new_line();
      this.key_triggered_button( "Rearrange Balls",  [ "b" ], () => { 
          this.ball_positions = Array( 40 ).fill( 0 ).map( x => Vec.of( 0,0,-10 ).randomized( 30 ).mult_pairs([ 1,1,2 ]) ).filter( p => p.norm() > 2.5 );
          this.balls = this.ball_positions.map( p => ({ position: p, color: Color.of( .5,.5,.5,.5 ).randomized( 1 ).normalized() }) );
          }, "Tan" );
    }
  static get_projection_parameters_from_matrix( m )  // Reverse engineer a projection matrix.
    { if( Vec.of( 0,0,0,1 ).equals( m[3] ) )         // Check the projection matrix for having orthographic form.
          { const right = 1/m[0][0], left = -right,
                    top = 1/m[1][1], bottom = -top,
                  c = m[2][2], d = m[2][3],
                  near = ( d+1 ) / c, far = ( d-1 ) / c;
            return { type:"orthographic", left, right, bottom, top, near, far };
          }
      else                                                            // Otherwise we have a perspective matrix.
          { const fov = 2 * Math.atan( 1/m[1][1] ), aspect = m[1][1] / m[0][0],
                  c = m[2][2], d = m[2][3],
                  near = d / ( c-1 ), far = d / ( c+1 );      // Notice the subtle difference from the orthographic case here.
            return { type:"perspective", fov, aspect, near, far };
          }
    }
  display( graphics_state, t = graphics_state.animation_time / 1000 )
    { graphics_state.projection_transform = this.drawn_graphics_state.projection_transform;
            // Even though we didn't need to reverse engineer the frustum at all to draw the normal (brightened) version of 
            // this scene that showcases the current projection matrix, we will still need the near and far plane values to draw
            // the dimmed version of the scene using a widened version of our real projection matrix.
      const parameters = Frustum_Demo.get_projection_parameters_from_matrix( graphics_state.projection_transform );

      // Iterate through a few different perspective matrices and draw the scene with each of them - brighter for the first (real) one.
      const frustums = [ graphics_state.projection_transform ];
      if( parameters.type == "perspective" )
        { frustums.push( Mat4.perspective( parameters.fov, parameters.aspect, parameters.near*.1, parameters.near ) )
          frustums.push( Mat4.perspective( parameters.fov, parameters.aspect, parameters.far,     parameters.far*10 ) )
        }
      else
        { frustums.push( Mat4.orthographic( parameters.left, parameters.right, parameters.bottom, parameters.top, 
                                                                              parameters.near*.1, parameters.near ) );
          frustums.push( Mat4.orthographic( parameters.left, parameters.right, parameters.bottom, parameters.top, 
                                                                              parameters.far,     parameters.far*10 ) );
        }

      const brightnesses = [ 1, .05 * Math.sin(t*20) + .3, .3];
      
      for( let j = 0; j < 3; j++ )
        { const temp_graphics_state = new Graphics_State( graphics_state.camera_transform, frustums[j] );
          temp_graphics_state.lights = graphics_state.lights;

          super.display( temp_graphics_state, brightnesses[j] );
          for( let b of this.balls )
          { this.plastic.color = b.color.map( (x,i) => i == 3 ? brightnesses[j] : x );
            this.shapes.ball.draw( temp_graphics_state, Mat4.translation( b.position ), this.plastic );
          }
        }      
    }
}