window.Ball = window.classes.Ball =
class Ball      // These data members of a Ball below are automatically filled in for you from the text file, due to code that's already present in Ray_Tracer::parse_line().
{ constructor(               position, size, color, k_a, k_d, k_s, n, k_r, k_refract, refract_index   )
    { Object.assign( this, { position, size, color, k_a, k_d, k_s, n, k_r, k_refract, refract_index } )
      this.model_transform = Mat4.translation( this.position ).times( Mat4.scale( this.size ) );
      this.m_inv           = Mat4.inverse( this.model_transform );
    }
  intersect( ray, existing_intersection, minimum_dist )
    { // Given a ray, check if this Ball is in its path.  Its first argument is the ray, a key/value object with an origin and a direction as keys.  The next argument
      // is a record of the nearest intersection found so far (a Ball pointer, a t distance value along the ray, and a normal), updates it if needed, and returns it.
      // Only counts intersections that are at least a given distance ahead along the ray.      
      const ray_inObjectCoords               = { origin: this.m_inv.times( ray.origin ), dir: this.m_inv.times( ray.dir ) },
          dir_inObjectCoords_normSquared     = ray_inObjectCoords.dir.to3().dot( ray_inObjectCoords.dir.to3() ),
          dir_inObjectCoords_normSquared_inv = 1 / dir_inObjectCoords_normSquared,
          orig_dot_del                       = ray_inObjectCoords.origin.to3().dot( ray_inObjectCoords.dir.to3() ),
          discriminant                       = orig_dot_del * orig_dot_del - dir_inObjectCoords_normSquared * 
            ( ray_inObjectCoords.origin.to3().dot( ray_inObjectCoords.origin.to3() ) - 1); //B squared minus AC, ie. quadratic equation

        if( discriminant > 0 )
        { var discriminant_sqrt = Math.sqrt( discriminant ),
              hit_1 = -(orig_dot_del + discriminant_sqrt),        // hit_1 is necessarily < hit_2 since we're adding something positive.
              hit_2 = -(orig_dot_del - discriminant_sqrt);
          
          hit_1 *= dir_inObjectCoords_normSquared_inv;
          hit_2 *= dir_inObjectCoords_normSquared_inv;    
          
          if( hit_1 < minimum_dist ) hit_1 = hit_2;             // Use the lesser of the two, unless that would be a degenerately near (re-)hit.
          if( hit_1 < minimum_dist || hit_1 >= existing_intersection.distance ) return existing_intersection;

          existing_intersection.ball = this;                    // If we made it here, this is the closest intersection > minimum_dist so far, so keep it.
          existing_intersection.distance = hit_1;               // Record the normal here too.  Transform the normal back to world space coords:
          existing_intersection.normal = this.m_inv.transposed().times( ray_inObjectCoords.origin.plus( ray_inObjectCoords.dir.times(hit_1) ) ).to3().normalized();
          if ( existing_intersection.normal.dot( ray.dir.to3() ) > 0 )         // Point the normal inside the shape if the ray is coming from within.
            existing_intersection.normal.scale(-1);
        }
        return existing_intersection;
      }
}


window.Ray_Tracer = window.classes.Ray_Tracer =
class Ray_Tracer extends Scene_Component  // Read in a text file that describes the location of balls and lights, and draw the result using ray tracing.
{ constructor( context )                      // Textures and a hidden canvas are utilized to paint the pixel colors onto a Square somewhere that WebGL can show.
    { super( context );
      if( !context.globals.has_controls   ) context.register_scene_component( new Movement_Controls( context ) ); // This scene includes a couple other 
      if( !context.globals.has_info_table ) context.register_scene_component( new Global_Info_Table( context ) ); // often-helpful scenes.
      context.globals.graphics_state.projection_transform = Mat4.perspective( Math.PI/3, context.width/context.height, .2, 50 );
            
      Object.assign( this, { width: 32, height: 32, near: 1, left: -1, right: 1, bottom: -1, top: 1, ambient: Vec.of( .1,.1,.1 ),
                             balls: [], lights: [], samplers: [], curr_background_function: "color", background_color: Color.of( 0,0,0,1 ),
                             sample_index: 0, visible: true, scratchpad: document.createElement('canvas'), gl: context.gl,
                             shader: context.get_instance( Phong_Model ), context } );
      this.background_functions = this.background_functions_list();
      
      const shapes = { "square": new Square(),                    // For texturing with and showing the ray traced result
                       "sphere": new Subdivision_Sphere( 4 ) };   // For drawing with ray tracing turned off
      this.submit_shapes( context, shapes );
      this.display_frustums = [];
      this.customize( context );
      for( let d of this.display_frustums ) 
      { context.register_scene_component( d );
        context.register_scene_component( d.sampler );
      }
    }
  customize( context, graphics_state = context.globals.graphics_state )
    { const distribution = new Grid_Sampler( this.context, Vec.of( 128, 128 ), Math.PI/3 );
      const graphics_state_copy = Object.assign( new Graphics_State(), graphics_state,
                                                  { camera_transform: graphics_state.camera_transform.copy(), 
                                                projection_transform: Mat4.perspective( Math.PI/3, context.width/context.height, 1, 10 ) } );  
      this.display_frustums.push( new Image_On_Frustum( context, distribution, graphics_state_copy,
          Vec.of( 128, 128 ) ) );

      const td = this.display_frustums[0].control_panel.parentElement;                // Let's reorder the control buttons
      td.parentElement.insertBefore( td, td.parentElement.firstChild.nextSibling );   // to make the display_frustum's visible.      
    }
  background_functions_list()    // These convert a ray into a color even when no balls were struck by the ray.
    { return ( { 
        waves: ray => 
          Color.of( .5*Math.pow( Math.sin( 2*ray.dir[0] ), 4 ) + Math.abs( .5*Math.cos( 8*ray.dir[0] + Math.sin( 10*ray.dir[1] ) + Math.sin( 10*ray.dir[2] ) ) ),
                    .5*Math.pow( Math.sin( 2*ray.dir[1] ), 4 ) + Math.abs( .5*Math.cos( 8*ray.dir[1] + Math.sin( 10*ray.dir[0] ) + Math.sin( 10*ray.dir[2] ) ) ),
                    .5*Math.pow( Math.sin( 2*ray.dir[2] ), 4 ) + Math.abs( .5*Math.cos( 8*ray.dir[2] + Math.sin( 10*ray.dir[1] ) + Math.sin( 10*ray.dir[0] ) ) ), 1 ),
        lasers: ray =>
          { var u = Math.acos( ray.dir[0] ), v = Math.atan2( ray.dir[1], ray.dir[2] );
            return Color.of( 1 + .5 * Math.cos( ~~(20*u)  ), 1 + .5 * Math.cos( ~~(20*v) ), 1 + .5 * Math.cos( ~~(8*u) ), 1 );
          },
        mixture:       ray => this.background_functions["waves" ]( ray ).mult_pairs( this.background_functions["lasers"]( ray ) ).to4(1),
        ray_direction: ray => Color.of( Math.abs( ray.dir[ 0 ] ), Math.abs( ray.dir[ 1 ] ), Math.abs( ray.dir[ 2 ] ), 1 ),
        color:         ray => this.background_color
      } );
    }
  trace( ray, color_remaining, is_primary, is_shadow_ray = false )
    { //        Given a ray, return the color in that ray's path.  The ray either originates from the camera itself or from a secondary reflection or refraction off of a
      //        ball.  Call Ball's intersect() method on each ball to determine the nearest ball struck, if any, and perform vector math (the Phong reflection formula)
      //        using the resulting intersection record to figure out the influence of light on that spot.  Recurse for reflections and refractions until the final color
      //        is no longer significantly affected by more bounces.
      //
      //        Arguments besides the ray include color_remaining, the proportion of brightness this ray can contribute to the final pixel.  Only if that's still
      //        significant, proceed with the current recursion, computing the Phong model's brightness of each color.  When recursing, scale color_remaining down by k_r
      //        or k_refract, multiplied by the "complement" (1-alpha) of the Phong color this recursion.  Use argument is_primary to indicate whether this is the original
      //        ray or a recursion.  Use the argument light_to_check whenever a recursive call to trace() is done for computing a shadow ray.
      
      if( color_remaining.norm() < .3 )    return Color.of( 0, 0, 0, 1 );  // Each recursion, check if there's enough remaining potential for the pixel to be brightened.

      let closest_intersection = { distance: Number.POSITIVE_INFINITY, ball: null, normal: null }    // An empty intersection object
      
      for( let b of this.balls ) closest_intersection = b.intersect( ray, closest_intersection, is_primary ? 1 : .0001 );
      
      if( !closest_intersection.ball ) return this.color_missed_ray( ray ); 
      
      if ( is_shadow_ray  ) return closest_intersection.distance < 1 ? "Shadow! The light was blocked." : "No shadow here!";
      
      const intersection_point = ray.origin.plus( ray.dir.times( closest_intersection.distance ) );
           
      const eye = ray.origin.minus( intersection_point ).normalized().to3();
      let color = closest_intersection.ball.color.times( closest_intersection.ball.k_a );
      for( let light of this.lights )
      { const L =  light.position.minus( intersection_point ).normalized().to3();

        const newRay = { origin: intersection_point, dir: light.position.minus( intersection_point ) };
        if ( this.trace( newRay, Vec.of( 1,1,1 ), false, true ) === "Shadow! The light was blocked." ) continue;     // Is shadow ray blocked?
        const H = L.plus( eye ).normalized();                     // **** If we're here, we're not in shadow, so apply Phong shading: ****
                      
        color = color.plus( light.color.to3().mult_pairs( 
                closest_intersection.ball.color.times( closest_intersection.ball.k_d *           Math.max( L.dot( closest_intersection.normal ), 0 ) ).plus
                              ( Vec.of( 1,1,1 ).times( closest_intersection.ball.k_s * Math.pow( Math.max( H.dot( closest_intersection.normal ), 0 ), closest_intersection.ball.n ) ) ) ) );            
      }                                                         
      for( let i = 0; i < 3; i++ ) { if( color[i] > 1 ) color[i] = 1;  if( color[i] < 0 ) color[i] = 0; }  // Cap lights' contributions to 1.
      
      const normal_dot_ray = closest_intersection.normal.dot( ray.dir.to3().normalized() ),
            cosTheta2 = Math.sqrt( 1 - closest_intersection.ball.k_refract * closest_intersection.ball.k_refract * ( 1 - normal_dot_ray * normal_dot_ray ) ),
            refraction_trajectory = ray.dir.to3().normalized().times(  closest_intersection.ball.refract_index ).plus( 
                                          closest_intersection.normal.times( -closest_intersection.ball.refract_index * normal_dot_ray - Math.sqrt(
                                         1 - closest_intersection.ball.refract_index * closest_intersection.ball.refract_index * ( 1 - normal_dot_ray * normal_dot_ray ) ) ) ),
            reflection_trajectory = closest_intersection.normal.times( 2 * normal_dot_ray ).minus( ray.dir.to3().normalized() ).times( normal_dot_ray < 0 ? -1 : 1 );
      
      const recursion_result = this.trace( { origin: intersection_point, dir: reflection_trajectory.to4(0) }, 
                                       color_remaining.mult_pairs( Vec.of( 1,1,1 ).minus( color ) ).times( closest_intersection.ball.k_r ),
                                     false ).to3().times( closest_intersection.ball.k_r ).plus(
                               this.trace( { origin: intersection_point, dir: refraction_trajectory.to4(0) }, 
                                             color_remaining.mult_pairs( Vec.of( 1,1,1 ).minus( color ) ).times( closest_intersection.ball.k_refract ),
                                           false ).to3().times( closest_intersection.ball.k_refract ) )
                              .mult_pairs( Vec.of( 1,1,1 ).minus( color ) );
      
      return color.plus( recursion_result ).to4(1);         // Add the 4th alpha component for display.
    }
  color_missed_ray( ray ) 
  { const answer = this.background_functions[ this.curr_background_function ] ( ray );
    return answer.mult_pairs( this.ambient ).to4(1);
  }


// TODO:  Move off all file reading functions into their own Scene_Reader Scene_Component, which Ray_Tracer registers by default.  It stores everything it reads into context.globals.balls and context.globals.res, etc.  Once this is done, Grid_Sampler and Image_From_Sampler_Scene should both consult globals.resolution and if present, use it during configure() instead of the argument passed.  For the latter, try moving the setup that uses resolution into its configure().


  parse_line( tokens )            // Load the lines from the textbox into variables.
    { for( let i = 1; i < tokens.length; i++ ) tokens[i] = Number.parseFloat( tokens[i] );
      switch( tokens[0] )
        { case "NEAR":    this.near   = tokens[1];  break;
          case "LEFT":    this.left   = tokens[1];  break;
          case "RIGHT":   this.right  = tokens[1];  break;
          case "BOTTOM":  this.bottom = tokens[1];  break;
          case "TOP":     this.top    = tokens[1];  break;
    //      case "RES":     this.width             = tokens[1];   this.height            = tokens[2];
    //                      this.scratchpad.width  = this.width;  this.scratchpad.height = this.height; break;
          case "SPHERE":
            this.balls.push( new Ball( Vec.of( tokens[1],tokens[2],tokens[3] ), Vec.of( tokens[4],tokens[5],tokens[6] ), Vec.of( tokens[7],tokens[8],tokens[9] ),
                                        tokens[10],tokens[11],tokens[12],  tokens[13],tokens[14],tokens[15],tokens[16] ) ); break;
          case "LIGHT":   this.lights.push( new Light( Vec.of( tokens[1],tokens[2],tokens[3], 1 ), Color.of( tokens[4],tokens[5],tokens[6], 1 ), 10000000 ) ); break;
          case "BACK":    this.background_color = Color.of( tokens[1],tokens[2],tokens[3], 1 ); this.gl.clearColor.apply( this.gl, this.background_color    ); break;
          case "AMBIENT": this.ambient = Vec.of( tokens[1], tokens[2], tokens[3] );
        }
    }
  parse_file()                                          // Turn the text in the textbox into local data members.  Move through the text lines:
    { this.balls = [];   this.lights = [];
      this.sample_index = 0; this.samples_per_frame = 1;                        // Begin at bottom scanline, forget the last image's speedup factor
      document.getElementById("progress").style = "display:inline-block;";        // Re-show progress bar
      const input_lines = document.getElementById( "input_scene" ).value.split("\n");
      for( let i of input_lines ) this.parse_line( i.split(/\s+/) );
    }
  load_case( i ) {   document.getElementById( "input_scene" ).value = Test_Cases.data()[ i ];   }
  make_control_panel() 
    { this.control_panel.innerHTML += "Open some test cases with the blue button.  See their text below:<br>";
      this.control_panel.appendChild( Object.assign( document.createElement( "textarea" ), { id:'input_scene', style:'white-space:nowrap;overflow-x:scroll;width:650px;height:250px;' } ) );  this.new_line();
      this.key_triggered_button( "Toggle Ray Tracing", [ "Alt","r"], function() { this.toggle_visible(); }, "#AF4C50" );
      this.control_panel.appendChild( Object.assign( document.createElement( "div" ), { id:'progress', style:'display:none;' } ) );  this.new_line();
      this.key_triggered_button( "Select Background Effect", ["b"], function() { document.getElementById("background_list").classList.toggle("show"); return false; }, "#8A8A4C" );
      this.key_triggered_button( "Select Test Case", ["t"], function() { document.getElementById("testcase_list").classList.toggle("show"); return false; }, "#4C50AF" );
      this.control_panel.appendChild( Object.assign( document.createElement( "div" ), { id:'testcase_list', className:'dropdown-content' } ) ); this.new_line();
      this.control_panel.appendChild( Object.assign( document.createElement( "div" ), { id:'background_list', className:'dropdown-content' } ) );
      this.key_triggered_button( "Submit Scene Textbox", ["Alt","s"], this.parse_file, "#3e8e41" );
            
      for( let i in Test_Cases.data() )
        { let a = document.createElement( "a" );
          a.addEventListener( "click", function() { this.load_case( i ); this.parse_file(); }.bind( this    ), false);
          a.innerHTML = i;
          document.getElementById( "testcase_list"  ).appendChild( a );
        }
      for( let j in this.background_functions )
        { let a = document.createElement( "a" );
          a.addEventListener( "click", function() { this.curr_background_function = j;      }.bind( this, j ), false);
          a.innerHTML = j;
          document.getElementById( "background_list" ).appendChild( a );
        }
      
      document.getElementById( "input_scene" ).addEventListener( "keydown", function(event) { event.cancelBubble = true; }, false );
      
      window.addEventListener( "click", function(event) {  if( !event.target.matches('button') ) {
        document.getElementById( "testcase_list"  ).classList.remove("show");
        document.getElementById( "background_list" ).classList.remove("show"); } }, false );
        
      this.load_case( "test_reflection" );
      this.parse_file();
    }
  toggle_visible()
                          // TODO:  Remove samplers from displayables list if visible is false.
  
    { this.visible = !this.visible; document.getElementById("progress").style = "display:inline-block;" }
  show_explanation( document_element )
    { document_element.innerHTML += `<p>This demo shows a ray tracer implemented in JavaScript.  It reads in a text field that describes the location of balls and lights, and draws the result using CPU ray tracing.  

</p><p>To use the Ray_Tracer class, one must provide:  1.  A Sampler, which defines the distribution of ray directions coming out of the eye, and 2.  A Scene_Component class that defines how to display onscreen all the colors resulting from sampling the rays, in whatever (possibly irregular) distribution a Sampler specifies.  The default Sampler is a grid pattern in screenspace, and these points are used on the near plane to project and define rays in world space.  The default displayer Scene employs HTML Image objects, textures, and a hidden canvas to paint the pixel colors onto a Square somewhere that WebGL can show.

</p><p>This code is structured like an assignment, with parts marked \"TODO\" in the code comments for you to figure out as an exercise.  To try the assignment yourself, press the blue button below and choose the final test case called \"show_homework_spec\" in the list, which prints out the whole assignment's instructions.  As of 12/11/17 the solution is now provided below each of the TODO code comments for completeness.</p>`;
    }
  take_samples( dt, samples_per_frame = 1 )
    { this.total_samples = this.display_frustums.reduce( ( acc, x ) => { return acc + x.sampler.rays.length; }, 0 );

      const desired_milliseconds_per_frame = 100;   // Based on dt (the time since last frame) compute how many samples we have time to take this frame.
      const milliseconds_per_sample = Math.max( dt / this.samples_per_frame, 1 );
      this.samples_per_frame = desired_milliseconds_per_frame / milliseconds_per_sample + 1;

      for( let i = 0; i < this.samples_per_frame; i++ )     // This lets us loop that many pixels, to update about a scanline's worth of pixels this frame:
        { if( ++this.sample_index >= this.total_samples ) { this.sample_index = 0; document.getElementById("progress").style = "display:none" };

          let [ s, index ] = [ 0, this.sample_index ];
          while( index >= this.display_frustums[s].sampler.rays.length ) index -= this.display_frustums[s++].sampler.rays.length;

          document.getElementById("progress").textContent = "Rendering ( " + 100 * this.sample_index / this.total_samples + "% )..."; 
          const direction = this.display_frustums[s].sampler.rays[ index ].worldspace_ray.to4(0),
                      ray = { origin: this.display_frustums[s].location.times( Vec.of( 0,0,0,1 ) ), dir: this.display_frustums[s].location.times( direction ) };   // Apply camera

          const result = this.trace( ray, Vec.of( 1,1,1 ), true )     // ******** Trace a single ray *********
          this.display_frustums[s].set_color( index, result );
        }
    }
  display( graphics_state )
    { graphics_state.lights = this.lights;
                                  // Raster the scene out of triangulated spheres without ray tracing, and draw
      for( let b of this.balls )  // them in world space in addition to the ray traced image placed in screen space.
        this.shapes.sphere.draw( graphics_state, b.model_transform, this.shader.material( b.color.to4(1), 
          { ambient: b.k_a, diffusivity: b.k_d, specularity: b.k_s, smoothness: b.n } ) );
      
      if( this.visible )
        this.take_samples( graphics_state.animation_delta_time );
      else 
        { this.sample_index = 0;    document.getElementById("progress").style = "display:none"; }
    }
}

window.Ray_Tracer_Performance = window.classes.Ray_Tracer_Performance =
class Ray_Tracer_Performance extends Ray_Tracer
  { customize( context, graphics_state = context.globals.graphics_state )
      { const distribution = new Grid_Sampler( this.context, Vec.of( 256, 256 ), Math.PI/3 );
        const graphics_state_copy = Object.assign( new Graphics_State(), graphics_state,
                                                    { camera_transform: graphics_state.camera_transform.copy(), 
                                                  projection_transform: Mat4.perspective( Math.PI/3, context.width/context.height, 1, 10 ) } );  
        this.display_frustums.push( new Image_On_Frustum( context, distribution, graphics_state_copy,
            Vec.of( 256, 256 ) ) );

        const td = this.display_frustums[0].control_panel.parentElement;                // Let's reorder the control buttons
        td.parentElement.insertBefore( td, td.parentElement.firstChild.nextSibling );   // to make the display_frustum's visible.      
      }
    make_control_panel() 
      { super.make_control_panel();
        this.load_case( "test_transparent" );
        this.parse_file();
      }
  }

window.Sampler = window.classes.Sampler =
class Sampler extends Scene_Component
{ constructor( context, FOV = Math.PI/3, aspect = 1.8 )
    { super( context );
      Object.assign( this, { context, rays: [], FOV, aspect } );
    }
  make_sampler_vectors()                          // Override this and make the rays, but be sure to call super.make_sampler_vectors().
    { this.initialization_hash = Math.random();

      // TODO: Sort the newly created this.rays in cache-friendly order here.
    }
}

window.Grid_Sampler = window.classes.Grid_Sampler =
class Grid_Sampler extends Sampler
{ constructor( context, resolution = Vec.of( 20, 20 ), FOV, aspect )
    { super( context, FOV, aspect );
      this.resolution = resolution;
      this.make_sampler_vectors();
    }
  make_sampler_vectors()
    { this.rays = [];
      for( let i = 0; i < this.resolution[0]; i++ ) for( let j = 0; j < this.resolution[1]; j++ )
        { const screenspace_pos = Vec.of( i/this.resolution[0]*2-1, j/this.resolution[1]*2-1 ),
                         spread = Vec.of( -this.FOV, -this.FOV / this.aspect ),
                          pixel = screenspace_pos.mult_pairs( spread );        
          this.rays.push( { screenspace_pos, color: Color.of( i%2,j%2,1,1 ),
                             worldspace_ray: Vec.of( Math.sin(pixel[0]) * Math.cos(pixel[1]),	Math.sin(pixel[1]), Math.cos(pixel[0]) * Math.cos(pixel[1]) ).times(-1) } );
        }
      super.make_sampler_vectors();
    }
  make_control_panel()
    { super.make_control_panel();
      this.key_triggered_button( "Raise resolution", ["Shift","p"], function() { this.resolution.scale(   1.1 ); this.configure(); } ); this.new_line();
      this.key_triggered_button( "Lower resolution",         ["p"], function() { this.resolution.scale( 1/1.1 ); this.configure(); } );
    }
}



window.Ray_Tracer_Frustum = window.classes.Ray_Tracer_Frustum =
class Ray_Tracer_Frustum extends Frustum_Tool
{ constructor( context, sampler, drawn_graphics_state )
    { super( context, drawn_graphics_state )
      Object.assign( this, { sampler, drawn_graphics_state } );
      this.location = Mat4.inverse( this.drawn_graphics_state.camera_transform );
    }
  take_over() { this.globals.movement_controls_target = () => this.drawn_graphics_state.camera_transform }
  join_camera() { this.saved_graphics_state = this.globals.graphics_state;
                  this.globals.graphics_state = this.drawn_graphics_state }
  decouple_camera() { this.globals.graphics_state = this.saved_graphics_state }
  set_color( index, color ) { this.sampler.rays[ index ].color = color }
  update() { }    // Override if changes to the Sampler rays should cause any changes to the display choices here.
  make_control_panel()
    { this.key_triggered_button( "Increase FOV", [ "Shift","v" ], function() { this.FOV  *= 1.1; this.configure(); } );
      this.key_triggered_button( "Decrease FOV", ["v"]      , function() { this.FOV  /= 1.1; this.configure(); } );      this.new_line();
      this.key_triggered_button( "Take over movement controls", ["1"], this.take_over     ); this.new_line();
      this.key_triggered_button( "Join camera to image",      ["1"], this.join_camera );   this.new_line();
      this.key_triggered_button( "Undo join camera to image", ["1"], this.decouple_camera );  this.new_line();
    }
  display( graphics_state )
    { super.display( graphics_state );
      this.location = Mat4.inverse( this.drawn_graphics_state.camera_transform );   // Update location once per frame
      if( this.sampler_update_status != this.sampler.initialization_hash )
          { this.update();                                                    // Update any info from the sampler we depend on 
            this.sampler_update_status  = this.sampler.initialization_hash;   // whenever the sampler changes
          }
                   // We'll draw the image to fill up the whole near plane.  Recreate the near plane of the current view
                   // frustum, by starting with the cannonical (cube) volume and undoing the projection.
      const near_plane_normalized = Vec.cast( [-1, -1, -1],  [1,  -1, -1],  [-1, 1, -1],  [1,  1, -1] );
      const near_plane_corners = Frustum_Demo.derive_frustum_points_from_matrix( this.drawn_graphics_state.projection_transform, near_plane_normalized );

      this.x_scale = near_plane_corners[1][0] - near_plane_corners[0][0];
      this.y_scale = near_plane_corners[2][1] - near_plane_corners[1][1];
      this.z_near  = near_plane_corners[0][2];
    }
}

window.Image_On_Frustum = window.classes.Image_On_Frustum =
class Image_On_Frustum extends Ray_Tracer_Frustum
{ constructor( context, sampler, drawn_graphics_state, resolution )
    { super( context, sampler, drawn_graphics_state )
      Object.assign( this, { resolution, phong_shader: context.get_instance( Phong_Model ) } );
      this.scratchpad = document.createElement('canvas');
      this.scratchpad_context = this.scratchpad.getContext('2d');             // A hidden canvas for assembling the texture
      this.scratchpad.width   = resolution[0];
      this.scratchpad.height  = resolution[1];
      this.imageData          = new ImageData( ...resolution );     // Will hold ray traced pixels waiting to be stored in the texture
      
      this.submit_shapes( context, { "square": new Square() } );    // For texturing with and showing the ray traced result
      
      this.texture = new Texture ( context.gl, "", false, false );           // Initial image source: Blank gif file
      this.texture.image.src = "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7";
    }
  set_color( index, color ) 
    { super.set_color( index, color );
      const pos = this.sampler.rays[ index ].screenspace_pos.plus( Vec.of( 1,1 ) ).times( .5 ).mult_pairs( this.resolution );
      const i = ~~pos[0] * this.resolution[1] + ~~pos[1];
    
      this.imageData.data[ 4 * i     ] = 255.9 * color[0];    
      this.imageData.data[ 4 * i + 1 ] = 255.9 * color[1];    
      this.imageData.data[ 4 * i + 2 ] = 255.9 * color[2];    
      this.imageData.data[ 4 * i + 3 ] = 255;
    }
  make_control_panel() 
    { super.make_control_panel();
      this.result_img = this.control_panel.appendChild( Object.assign( document.createElement( "img" ), 
                { style:"transform: rotate(270deg); margin: -50px 0 0 55px; width:200px; height:" + 200 * this.aspect_ratio + "px" } ) );
    }
  display( graphics_state )
    { super.display( graphics_state );
      if( !this.texture || !this.texture.loaded ) 
        return;      // Don't display until we've got our first procedural image

      this.scratchpad_context.putImageData( this.imageData, 0, 0 );         // Draw the image on the hidden canvas.
      this.texture.image.src = this.scratchpad.toDataURL("image/png");      // Convert the canvas back into an image and send to a texture.
         this.result_img.src = this.scratchpad.toDataURL("image/png");
      
      const square_matrix = Mat4.translation([ 0,0, this.z_near ])      // All of their z coordinates should match
                    .times( Mat4.rotation( Math.PI, Vec.of( 1,1,0 ) ) )   // Swap the corners' places so their texture coord order is correct.
                    .times( Mat4.scale([ this.y_scale/2, this.x_scale/2, 1 ]) );

      this.shapes.square.draw( graphics_state, this.location.times( square_matrix ),
             this.phong_shader.material( Color.of( 0,0,0,1 ), { ambient: 1, diffusivity: 0, specularity: 0, texture: this.texture } ) );
    }
}