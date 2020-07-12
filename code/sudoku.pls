-- PL/SQL Sudoku Solver
-- William Robertson 2007, www.williamrobertson.net
-- Improved version incorporating additional cross-hatching algorithm.
-- Used new 10g collection operator MEMBER OF as it simplifies coding (otherwise would have
-- to code additional loops to test each element in collection looking for match with value).


-- Allow optional parameter
set feed off
col 1 new_value 1
select null as "1" from dual where 1=2;
-- "&1" now has value passed on command line, or null

declare
   procedure drop_type (p_object_name varchar2)
   is
      -- Drop type, handling exception:
      -- ORA-04043: object xx does not exist
      no_such_type exception;
      pragma exception_init(no_such_type, -4043);
   begin
      execute immediate 'DROP TYPE ' || p_object_name || ' FORCE';
      dbms_output.put_line('Type ' || p_object_name || ' dropped.');
   exception
      when no_such_type then dbms_output.put_line('Type ' || p_object_name || ' not found.');
   end drop_type;
begin
   if upper('&1') = 'REBUILD' then
      drop_type('SUDOKU');
      drop_type('SUDOKU_ELEMENT_TT');
      drop_type('SUDOKU_ELEMENT');
      drop_type('SUDOKU_CELL_TT');
   else
      dbms_output.put_line
      ( 'Specify REBUILD on command line to drop sudoku types before attempting to create them.' );
      dbms_output.new_line();
   end if;
end;
/

set feed 1

-- Originally made SUDOKU_CELL_TT a VARRAY because it made sense to limit it to nine values,
-- however the 10g multiset operators (MEMBER OF, MULTISET EXCEPT etc) only work on nested table collections,
-- and SUDOKU_ELEMENT contains validation that prevents more values than this being supplied anyway, so I
-- decided a nested table collection type was better in this case.
create or replace type sudoku_cell_tt as table of number(1)
/

prompt Type SUDOKU_ELEMENT:
create or replace type sudoku_element
as object
( cells  sudoku_cell_tt
, member procedure check_valid  -- Checks for dups, raises error (can't use name "validate")
   ( self in out nocopy sudoku_element
   , p_label varchar2 default null )  -- Label for use in error message e.g. "Column 5"
, member procedure set_cell
   ( self in out nocopy sudoku_element
   , p_position positive
   , p_value positive )
, member function free_values     -- Returns the set of missing numbers for a set
   return sudoku_cell_tt
, member function free_locations  -- Returns the set of blank cell positions for a set
   return sudoku_cell_tt
, member procedure print
   ( self in out nocopy sudoku_element
   , p_name varchar2 default null  -- Optional label
   , p_gridlines boolean default true )  -- Optionally print gridlines
, constructor function sudoku_element
   ( p_cell1     positive
   , p_cell2     positive
   , p_cell3     positive
   , p_cell4     positive
   , p_cell5     positive
   , p_cell6     positive
   , p_cell7     positive
   , p_cell8     positive
   , p_cell9     positive
   , p_validate  boolean default true )
   return self as result
-- Convenient alternative constructor accepts a single character string e.g. '52 7 4  1'
, constructor function sudoku_element
   ( p_cell_string varchar2 )
   return self as result
-- Another convenient alternative constructor populates with all nulls:
, constructor function sudoku_element
   return self as result
)
/

show errors type sudoku_element

prompt Type Body sudoku_element:

create or replace type body sudoku_element
as
   member procedure check_valid
      ( self in out nocopy sudoku_element
      , p_label varchar2 default null )  -- Label for use in error message e.g. "Column 5"
   is
      v_available sudoku_cell_tt;
   begin
      -- Handy 10g collection member uniqueness check:
      -- (actually doesn't appear to help performance as I hoped it would)
      if self.cells is A set then
         return;
      else
         v_available := new sudoku_cell_tt(null,null,null,null,null,null,null,null,null);

         for i in self.cells.first..self.cells.last
         loop
            -- Place each element value into a blank set, checking for collisions:
            if self.cells(i) is null
            then
               null;
            elsif v_available(self.cells(i)) is not null
            then
               self.print(p_label);

               raise_application_error
               ( -20000
               , ltrim(p_label || ': ',': ') ||
                 'Value "' || self.cells(i) || '" is duplicated.'
               , true );
            else
               v_available(self.cells(i)) := self.cells(i);  -- Placeholder: this slot now taken
            end if;
         end loop;
      end if;
   end check_valid;


   member procedure set_cell
      ( self in out nocopy sudoku_element
      , p_position positive
      , p_value positive )
   is
   begin
      self.cells(p_position) := p_value;

      -- Only need check during development, otherwise unnecessary overhead:
      -- check_valid;
   end set_cell;


   -- Values not in self.cells
   -- (In 10g we can replace procedural loops with multiset except x y, where x is the full set 1-9 and y is self.cells)
   member function free_values
      return sudoku_cell_tt
   is
      -- Start with the whole set, then remove those in use
      -- v_free sudoku_cell_tt := sudoku_cell_tt(1,2,3,4,5,6,7,8,9);

      -- n pls_integer := 1;
   begin
      -- ms operator takes multiset 2 away from multiset 1, in this case [all numbers] minus [those we have used so far],
      -- to return [unused numbers], i.e. possible values for an unresolved cell.
      return sudoku_cell_tt(1,2,3,4,5,6,7,8,9) multiset except self.cells;
   end free_values;


   member function free_locations  -- Returns the set of blank cell positions for a set
      return sudoku_cell_tt
   is
      v_locations sudoku_cell_tt := sudoku_cell_tt();
   begin
      for i in self.cells.first..self.cells.last loop
         if self.cells(i) is null then
            v_locations.extend;
            v_locations(v_locations.count) := i;
         end if;
      end loop;

      return v_locations;
   end free_locations;


   member procedure print
      ( self in out nocopy sudoku_element
      , p_name varchar2 default null  -- Optional label
      , p_gridlines boolean default true ) -- Optionally print gridlines
   is
   begin
      dbms_output.put(rpad(nvl(p_name,'Element')||':',9));

      for i in 1..9 loop
         dbms_output.put(lpad(nvl(to_char(self.cells(i)),'-'),3));
         if p_gridlines and i in (3,6) then
            dbms_output.put('  |');
         end if;
      end loop;

      dbms_output.new_line();
   end print;


   constructor function sudoku_element
      ( p_cell1     positive
      , p_cell2     positive
      , p_cell3     positive
      , p_cell4     positive
      , p_cell5     positive
      , p_cell6     positive
      , p_cell7     positive
      , p_cell8     positive
      , p_cell9     positive
      , p_validate  boolean default true )  -- Allowed to skip validation
      return self as result                 -- e.g. when constructing from previously checked values
   is
   begin
      self.cells :=
         new sudoku_cell_tt(p_cell1,p_cell2,p_cell3,p_cell4,p_cell5,p_cell6,p_cell7,p_cell8,p_cell9);

      if p_validate then
         check_valid();
      end if;

      return;
   end sudoku_element;


   constructor function sudoku_element
      ( p_cell_string varchar2 )
      return self as result
   is
      v_cell_string char(10) := rpad(nvl(p_cell_string,' '),10);
   begin
      self :=
         new sudoku_element
         ( nullif(substr(v_cell_string,1,1),' ')
         , nullif(substr(v_cell_string,2,1),' ')
         , nullif(substr(v_cell_string,3,1),' ')
         , nullif(substr(v_cell_string,4,1),' ')
         , nullif(substr(v_cell_string,5,1),' ')
         , nullif(substr(v_cell_string,6,1),' ')
         , nullif(substr(v_cell_string,7,1),' ')
         , nullif(substr(v_cell_string,8,1),' ')
         , nullif(substr(v_cell_string,9,1),' ') );

      return;
   end sudoku_element;


   constructor function sudoku_element
   return self as result
   is
   begin
      self.cells := new sudoku_cell_tt(null,null,null,null,null,null,null,null,null);
      return;
   end sudoku_element;

end;
/
show errors type body sudoku_element


-- Array type, 9 x sudoku elements:
prompt Type sudoku_element_tt (varray(9) of sudoku_element):
create or replace type sudoku_element_tt as varray(9) of sudoku_element;
/

show errors type sudoku_element_tt


prompt Type sudoku:
create or replace type sudoku as object
( s_rows sudoku_element_tt      -- Array of 9 Sudoku Element objects (can't use name "rows")
, status varchar2(8)            -- 'New'|'Solved'|'Unsolved'
, member function s_columns     -- Getter function for a single specified column object
      ( p_col# integer )
      return sudoku_element
, member function s_squares     -- Getter function for a single specified square object
      ( p_square# integer )
       return sudoku_element
, member procedure print
      ( p_gridlines boolean default true )
, member procedure solve
      ( p_verbose boolean default false )
, member function solved_cursor  -- Results as ref cursor
  return sys_refcursor
, constructor function sudoku
      ( p_rows sudoku_element_tt
      , p_verbose boolean default false )
      return self as result
, constructor function sudoku
      ( p_row1 varchar2 default '         '
      , p_row2 varchar2 default '         '
      , p_row3 varchar2 default '         '
      , p_row4 varchar2 default '         '
      , p_row5 varchar2 default '         '
      , p_row6 varchar2 default '         '
      , p_row7 varchar2 default '         '
      , p_row8 varchar2 default '         '
      , p_row9 varchar2 default '         '
      , p_verbose boolean default false )
      return self as result
);
/

show errors type sudoku

prompt Type body sudoku:
create or replace type body sudoku
as
   member procedure print
      ( p_gridlines boolean default true )
   is
      k_horizontal_rule constant varchar2(50) := '-          ---------+-----------+----------';
   begin
      dbms_output.put_line(k_horizontal_rule);
      for i in 1..9 loop
         self.s_rows(i).print('Row ' || i);
         if p_gridlines and i in (3,6) then
            dbms_output.put_line(k_horizontal_rule);
         end if;
      end loop;

      if p_gridlines then
         dbms_output.put_line(k_horizontal_rule);
      end if;
      dbms_output.new_line();
   end print;


   -- Getter function for a single specified column:
   member function s_columns
      ( p_col# integer )
      return sudoku_element
   is
   begin
      return new sudoku_element
         ( self.s_rows(1).cells(p_col#)
         , self.s_rows(2).cells(p_col#)
         , self.s_rows(3).cells(p_col#)
         , self.s_rows(4).cells(p_col#)
         , self.s_rows(5).cells(p_col#)
         , self.s_rows(6).cells(p_col#)
         , self.s_rows(7).cells(p_col#)
         , self.s_rows(8).cells(p_col#)
         , self.s_rows(9).cells(p_col#)
         , false );  -- Don't re-validate
   end s_columns;


   -- Getter function for a single specified square:
   member function s_squares
      ( p_square# integer )
      return sudoku_element
   is
      k_startrow constant pls_integer := ceil(p_square#/3) * 3 -2;
      k_startcol constant pls_integer := mod(p_square# -1,3) * 3 + 1;
   begin
      return new sudoku_element
         ( self.s_rows(k_startrow).cells(k_startcol)
         , self.s_rows(k_startrow).cells(k_startcol +1)
         , self.s_rows(k_startrow).cells(k_startcol +2)

         , self.s_rows(k_startrow +1).cells(k_startcol)
         , self.s_rows(k_startrow +1).cells(k_startcol +1)
         , self.s_rows(k_startrow +1).cells(k_startcol +2)

         , self.s_rows(k_startrow +2).cells(k_startcol)
         , self.s_rows(k_startrow +2).cells(k_startcol +1)
         , self.s_rows(k_startrow +2).cells(k_startcol +2)

         , false );  -- Don't re-validate
   end s_squares;


   -- Approach is to find intersection of free lists for current row, column and square.
   -- Row, column and square are sudoku_element objects, which have a free_values method,
   -- Thus for each null cell we will look for
   -- intersect(row.free_values, column.free_values, square.free_values)  -- pseudo-code
   -- and hope for a single value, i.e. candidates.count = 1.
   member procedure solve
      ( p_verbose boolean default false )
   is
      sudoku_broken exception;
      pragma exception_init(sudoku_broken, -20200);

      procedure put_line ( p_text varchar2 ) is
      begin
         if p_verbose then dbms_output.put_line(p_text); end if;
      end put_line;

      procedure put ( p_text varchar2 ) is
      begin
         if p_verbose then dbms_output.put(p_text); end if;
      end put;

      procedure new_line is
      begin
         if p_verbose then dbms_output.new_line; end if;
      end new_line;


      -- Given two sudoku_element objects, return the set of common values:
      -- Third set is optional
      function intersection
         ( p_set1 sudoku_cell_tt
         , p_set2 sudoku_cell_tt
         , p_set3 sudoku_cell_tt default null )
         return sudoku_cell_tt
      is
         v_result sudoku_cell_tt;
      begin
         if p_set1.count = 0 or p_set2.count = 0 then
            v_result := sudoku_cell_tt();
         else
            v_result := (p_set1 multiset intersect p_set2);
         end if;

         if p_set3 is not null then
            v_result := (v_result multiset intersect p_set3);
         end if;

         return v_result;
      end intersection;


      -- Determine which square a (row,column) coordinate falls in:
      function which_square
         ( p_row# pls_integer
         , p_col# pls_integer )
         return pls_integer
      is
      begin
         return ceil(p_col#/3) +
         case
            when p_row# <= 3 then 0
            when p_row# <= 6 then 3
            when p_row# <= 9 then 6
         end;

         -- Could also use:
         -- ceil(p_col#/3) + ((ceil(p_row#/3) -1) *3)
         -- but this is harder to read and I suspect less efficient (untested)
      end which_square;


      -- "Locations" check:
      -- For a specified value and row, return check which cell(s) it can go in.
      -- e.g. Looking at a particular cell, perhaps any of {2,3,5} could go in it. However, maybe 2 cannot go in the
      -- row above because there is already  a 2 in that row, etc. so in fact this cell is the only place for that 2.
      -- Return true if the position specified is the only valid position for this value in the row.
      function only_valid_location_in_row
         ( p_candidate  pls_integer      -- A candidate value to check e.g. 4
         , p_position   pls_integer      -- A position in the row
         , p_which_row  pls_integer      -- the row number, e.g. row 6
         , p_row        sudoku_element ) -- the row object itself
         return boolean
      is
         v_locations    sudoku_cell_tt := p_row.free_locations(); -- Possible locations within this row.
         v_which_column pls_integer;
         v_which_square pls_integer;
         v_results      sudoku_cell_tt := sudoku_cell_tt();
      begin
         if p_candidate is null then
            -- Cheap shortcut simplifies validation - null in, null out:
            return null;
         elsif p_candidate not member of p_row.free_values then
            -- Review how we want to handle this. Is it an error or something we can just skip?
            raise_application_error
            ( -20000
            , 'Value ' || p_candidate || ' is already in use within row ' || p_which_row );

         elsif v_locations.count > 9 then
            raise_application_error
            ( -20000
            , 'valid_locations_in_row: value ' || p_candidate || ' has ' || v_locations.count || ' possible locations in row ' || p_which_row || '.'  );
         end if;

         -- The specified row will have two or more unused values to place.
         -- This procedure is passed one of those values as p_candidate.
         -- Build up a collection of possible locations for it.
         -- e.g. row is {4,2,1,null,9,7,6,8,null}, and p_candidate is 3.
         -- Initially 3 could go in positions 4 or 9 (the two empty locations).
         -- However, it is possible that column 4 already has the value 3, in which case it must go in column 9.
         -- The corresponding square is checked in a similar way.

         -- Loop through the row's empty locations, checking whether the candidate can go in each one:
         <<locations>>
         for l in v_locations.first..v_locations.last
         loop
            v_which_column := v_locations(l);  -- The current location to check
            v_which_square := which_square(p_which_row,v_which_column);

            -- See whether this value is also free within the column:
            if  p_candidate member of self.s_columns(v_which_column).free_values
            and p_candidate member of self.s_squares(v_which_square).free_values
            then
               -- This is a possible location for this value:
               -- [todo: break out of loop after finding second possible location. For now, confirm correct.]
               -- v_locations_count := v_locations_count +1;
               v_results.extend;
               v_results(v_results.count) := v_which_column;
               exit when v_results.count > 1;
            end if;

         end loop locations;

         -- true if there is only one position and it is the specified one:
         return v_results.count = 1 and p_position member of v_results;
      end only_valid_location_in_row;


      -- "Locations" check2:
      -- For a specified value, position and column, see whether the specified posiition is the only place it can go.
      function only_valid_location_in_column
         ( p_candidate     pls_integer      -- A candidate value to check e.g. 4
         , p_position      pls_integer      -- A position in the row
         , p_which_column  pls_integer      -- the column number, e.g. column 6
         , p_column        sudoku_element ) -- the column object itself
         return boolean
      is
         v_locations  sudoku_cell_tt := p_column.free_locations(); -- Possible locations within this column.
         v_which_row    pls_integer;
         v_which_square pls_integer;
         v_results sudoku_cell_tt := sudoku_cell_tt();
      begin
         if p_candidate is null then
            -- Cheap shortcut simplifies validation - null in, null out:
            return null;
         elsif p_candidate not member of p_column.free_values then
            -- Review how we want to handle this. Is it an error or something we can just skip?
            raise_application_error
            ( -20000
            , 'Value ' || p_candidate || ' is already in use within column ' || p_which_column );

         elsif v_locations.count > 9 then
            raise_application_error
            ( -20000
            , 'valid_locations_in_row: value ' || p_candidate || ' has ' || v_locations.count || ' possible locations in row ' || p_which_column || '.');
         end if;

         -- Loop through the column's empty locations, checking whether the candidate can go in each one:
         <<locations>>
         for l in v_locations.first..v_locations.last
         loop
            v_which_row := v_locations(l);  -- The current location to check
            v_which_square := which_square(v_which_row,p_which_column);

            -- See whether this value is also free within the column:
            if  p_candidate member of self.s_rows(v_which_row).free_values
            and p_candidate member of self.s_squares(v_which_square).free_values
            then
               -- This is a possible location for this value:
               v_results.extend;
               v_results(v_results.count) := v_which_row;

               -- We are checking for the case where the value can only go in one position, so no point continuing if > 1:
               exit when v_results.count > 1;
            end if;

         end loop locations;

         -- true if there is only one position and it is the specified one:
         return v_results.count = 1 and p_position member of v_results;
      end only_valid_location_in_column;



      -- "Locations" check2:
      -- For a specified value and column, return the only cell(s) it can go in.
      function valid_locations_in_column
         ( p_candidate     pls_integer      -- A candidate value to check e.g. 4
         , p_which_column  pls_integer      -- the column number, e.g. column 6
         , p_column        sudoku_element ) -- the column object itself
         return sudoku_cell_tt
      is
         v_locations  sudoku_cell_tt := p_column.free_locations(); -- Possible locations within this column.
         v_which_row    pls_integer;
         v_which_square pls_integer;
         v_results sudoku_cell_tt := sudoku_cell_tt();
      begin
         if p_candidate is null then
            -- Cheap shortcut simplifies validation - null in, null out:
            return null;
         elsif p_candidate not member of p_column.free_values then
            -- Review how we want to handle this. Is it an error or something we can just skip?
            raise_application_error
            ( -20000
            , 'Value ' || p_candidate || ' is already in use within column ' || p_which_column );

         elsif v_locations.count > 9 then
            raise_application_error
            ( -20000
            , 'valid_locations_in_row: value ' || p_candidate || ' has ' || v_locations.count || ' possible locations in row ' || p_which_column || '.' );
         end if;

         -- Loop through the column's empty locations, checking whether the candidate can go in each one:
         <<locations>>
         for l in v_locations.first..v_locations.last
         loop
            v_which_row := v_locations(l);  -- The current location to check
            v_which_square := which_square(v_which_row,p_which_column);

            -- See whether this value is also free within the column:
            if  p_candidate member of self.s_rows(v_which_row).free_values
            and p_candidate member of self.s_squares(v_which_square).free_values
            then
               -- This is a possible location for this value:
               v_results.extend;
               v_results(v_results.count) := v_which_row;

               -- We are checking for the case where the value can only go in one position, so no point continuing if > 1:
               exit when v_results.count > 1;
            end if;

         end loop locations;

         return v_results;
      end valid_locations_in_column;


      -- "Locations" check3:
      -- For a specified value,position and square, see whether the specified posiition is the only place it can go.
      function only_valid_location_in_square
         ( p_candidate     pls_integer      -- A candidate value to check e.g. 4
         , p_position      pls_integer      -- A position in the row
         , p_which_square  pls_integer      -- the column number, e.g. column 6
         , p_square        sudoku_element ) -- the column object itself
         return boolean
      is
         v_locations  sudoku_cell_tt := p_square.free_locations();  -- Possible locations within this column.

         v_which_row     pls_integer;
         v_which_column  pls_integer;
         v_results sudoku_cell_tt := sudoku_cell_tt();

         -- Offsets for starting loop over specified square
         -- e.g. square 6 starts at row 4, column 7
         -- (allow column offset to start at 1 instead of more logical 0 because easier for adding to
         -- mod() expression that returns 0/1/2).
         k_rowoffset     constant pls_integer := ceil(p_which_square/3) * 3 -3;
         k_column_offset constant pls_integer := mod(p_which_square -1,3) * 3 +1;
      begin
         if p_candidate is null then
            -- Cheap shortcut simplifies validation - null in, null out:
            return null;
         elsif p_candidate not member of p_square.free_values then
            -- Review how we want to handle this. Is it an error or something we can just skip?
            raise_application_error
            ( -20000
            , 'Value ' || p_candidate || ' is already in use within square ' || p_which_square );

         elsif v_locations.count > 9 then
            raise_application_error
            ( -20000
            , 'valid_locations_in_square: value ' || p_candidate || ' has ' || v_locations.count || ' possible locations in square ' || p_which_square || '.' );
         end if;

         -- Loop through the column's empty locations, checking whether the candidate can go in each one:
         <<locations>>
         for l in v_locations.first..v_locations.last
         loop
            -- Map location n of specified square to a row and column:
            v_which_row    := ceil(v_locations(l)/3) + k_rowoffset;
            v_which_column := mod(v_locations(l) -1,3) + k_column_offset;

            -- See whether this value is also free within the row and column:
            -- (optimization possible - currently repeat each row and column lookup 3 times)
            if  p_candidate member of self.s_rows(v_which_row).free_values
            and p_candidate member of self.s_columns(v_which_column).free_values
            then
               -- This is a possible location for this value:
               v_results.extend;
               v_results(v_results.count) := v_locations(l);

               -- We are checking for the case where the value can only go in one position, so no point continuing if > 1:
               exit when v_results.count > 1;
            end if;

         end loop locations;

         -- true if there is only one position and it is the specified one:
         return v_results.count = 1 and p_position member of v_results;
      end only_valid_location_in_square;


      -- For a given coordinate pair, find the set of possible values from the intersection of
      -- row.free_values, column.free_values and square.free_values. If only one possible value, return it.
      function unique_value
         ( p_row# pls_integer
         , p_col# pls_integer
         , p_square# pls_integer
         , p_row sudoku_element
         , p_column sudoku_element
         , p_square sudoku_element )
         return pls_integer
      is
         v_candidates sudoku_cell_tt;
         v_locations sudoku_cell_tt;
         v_result pls_integer;
         v_position_in_square pls_integer := mod(p_row# -1,3) * 3 + mod(p_col# -1,3) + 1;
      begin
         v_candidates :=
            intersection
            ( p_row.free_values
            , p_column.free_values
            , p_square.free_values );

         -- nb relies on intersection returning consecutive non-null values
         -- so "count = 1" means we want the first value, avoiding need for loop.
         -- This would break if e.g. first value is null and count = 2.
         if v_candidates.count = 0 then
            -- This exception is handled by caller, which abandons current retry and moves on to next:
            raise_application_error
            ( -20200
            , 'Invalid sudoku: cell at row ' || p_row# || ' column ' || p_col# || ' has no possible values.');

         elsif v_candidates.count = 1 then
            put('Intsc: ');
            v_result := v_candidates(v_candidates.first);

         elsif v_candidates.count > 1
         then
            -- Compile with extended diagnostics using conditional compilation:
            -- alter type sudoku compile body plsql_ccflags = 'extended_diagnostics:true' reuse settings;
            -- or at session level:
            -- alter session set plsql_ccflags = 'extended_diagnostics:true';
            $if $$extended_diagnostics $then
               -- See whether we can eliminate some more with Plan B (Possible Locations algorithm):
               put('Row ' || p_row# || ' col ' || p_col# || ': ' || v_candidates.count || ' candidates:');

               if p_verbose then -- Parameter to solve() procedure, default false
                  -- Dump out possible values for unresolved cells, if verbose specified:
                  for i in v_candidates.first..v_candidates.last loop
                     put(lpad(v_candidates(i),3));
                  end loop;
                  new_line();
               end if;
            $end

            for i in v_candidates.first..v_candidates.last loop
               -- Try each candidate to see whether all other locations can be eliminated:

               if only_valid_location_in_row(v_candidates(i), p_col#, p_row#, p_row)
               then
                  put('Loc-R: ');
                  v_result := v_candidates(i);

               elsif only_valid_location_in_column(v_candidates(i), p_row#, p_col#, p_column)
               then
                  put('Loc-C: ');
                  v_result := v_candidates(i);

               else
                  /*
                  put
                  ( 'only_valid_location_in_square: row ' || p_row# ||
                    ', column ' || p_col# ||
                    ', square ' || p_square# ||
                    ', position ' || v_position_in_square ||
                    ', value ' || v_candidates(i) || ': ' );
                  */

                  if only_valid_location_in_square(v_candidates(i), v_position_in_square, p_square#, p_square)
                  then
                     put('Loc-S: ');
                     v_result := v_candidates(i);
                  end if;
               end if;
            end loop;
         end if;

         return v_result;
      end unique_value;


      procedure solve_inner
         ( p_verbose boolean default false )
      is
         type per_iteration_stats_rectype is record
         ( checked     pls_integer
         , resolved    pls_integer
         , start_time  timestamp
         , end_time    timestamp );

         -- Table of stats, one row per iteration:
         type stats_array is table of per_iteration_stats_rectype index by pls_integer;

         v_stats stats_array;

         v_found_value pls_integer;
         v_sudoku_changed boolean := true;
         v_sudoku_solved  boolean := false;
         v_square# pls_integer;
         v_iteration pls_integer := 0;
      begin
         <<deductions>>
         while v_sudoku_changed  -- Starts out true
         and not v_sudoku_solved
         and v_iteration <= 81  -- Emergency brake - handy during development
         loop
            v_sudoku_changed := false;

            v_iteration := v_iteration +1;

            v_stats(v_iteration).start_time := systimestamp;
            v_stats(v_iteration).checked := 0;
            v_stats(v_iteration).resolved := 0;

            put_line('Iteration ' || v_iteration || ':');

            <<rows>>
            for rw in 1..9 loop
               <<cols>>
               for c in 1..9 loop
                  if self.s_rows(rw).cells(c) is null then
                     -- Keep count of number checked for stats:
                     v_stats(v_iteration).checked := v_stats(v_iteration).checked +1;

                     v_square# := which_square(rw,c);

                     -- Look for a value for this cell (function returns null if no unique value):
                     v_found_value :=
                     unique_value
                     ( rw
                     , c
                     , v_square#
                     , self.s_rows(rw)
                     , self.s_columns(c)
                     , self.s_squares(v_square#) );

                     if v_found_value is not null then
                        self.s_rows(rw).set_cell(c,v_found_value);

                        v_stats(v_iteration).resolved := v_stats(v_iteration).resolved +1;
                        v_sudoku_changed := true;

                        put_line('Row ' || rw || ' col ' || c || ' set to ' || v_found_value);
                     end if;
                  end if;
               end loop cols;
            end loop rows;

            -- Sudoku is solved if we resolved every unresolved cell this iteration:
            v_sudoku_solved := v_stats(v_iteration).checked = v_stats(v_iteration).resolved;

            put_line
            ( 'Checked ' || v_stats(v_iteration).checked ||
            ', resolved ' || v_stats(v_iteration).resolved || chr(10));

            v_stats(v_iteration).end_time := systimestamp;
         end loop deductions;

         if v_sudoku_solved then
            self.status := 'Solved';
         else
            self.status := 'Unsolved';
         end if;

         put_line(self.status || ' after ' || v_iteration || ' iterations.' || chr(10));
      end solve_inner;
   begin
      declare
         -- Local block to bring variable declarations nearer code:
         v_candidates sudoku_cell_tt;
         v_last_stable_sudoku sudoku;
         v_square# pls_integer;
         v_guesses pls_integer := 0;
         v_candidate_count_orig pls_integer := 0;
         v_what_if_restarts pls_integer := 0;
      begin
         dbms_application_info.set_module('Sudoku solver', 'Basic');
         dbms_application_info.set_client_info(null);

         new_line();
         put_line('"Intsc" = derived via Intersection check (only one possible value for this cell)');
         put_line('"Loc-R/C/S" = derived via possible-locations check (the value could not go anywhere else in its row/column/square)');
         put_line('"Guess" = The guessing ("What If?") test eliminated all other values as they resulted in an invalid sudoku'
         || chr(10));

         -- This *almost* always resolves the sudoku.
         solve_inner(p_verbose);

         dbms_application_info.set_action(self.status);

         if self.status = 'Solved' then
            dbms_application_info.set_client_info(null);
            return;
         else
            dbms_application_info.set_action('Phase 2');
            put_line('Best effort without guesswork:');
            if p_verbose then
               self.print();
            end if;
         end if;

         v_last_stable_sudoku := self;

         -- However, the above will abandon the attempt if every remaining blank cell could have
         -- two or more values, so we now retry with each candidate value. We only guess one value at a time -
         -- * If it leads to an invalid sudoku, it is wrong and can be eliminated. The sudoku reverts to its state
         --   prior to the guess and the candidate is removed from the list of candidates for that cell.
         -- * If it appears valid but fails to lead to a solution, it is backed out
         -- This is apparently an official technique known as "What If", "Guess-and-Check", "Bifurcation", "Backtracking" or "Ariadne's thread"
         -- http://en.wikipedia.org/wiki/Sudoku

         -- This outermost loop does nothing except allowing the whole "what if" cycle to restart from a stable point
         <<what_if_restarts>>
         while self.status != 'Solved' and v_what_if_restarts < 9 * 81 and v_guesses < 9 * 81 loop
            v_what_if_restarts := v_what_if_restarts +1;

            if p_verbose then
               if v_what_if_restarts = 1 then
                  put_line(chr(10) || 'Starting what-if tests');
               else
                  put_line(chr(10) || 'Restarting what-if tests (' || v_what_if_restarts || ')');
                  self.print();
               end if;
            end if;

            <<what_if>>
            for rw in 1..9 loop
               exit when self.status = 'Solved';

               put_line(chr(10) || 'Loop "what_if", row ' || rw);

               <<cell>>
               for c in 1..9 loop
                  exit when self.status = 'Solved';

                  dbms_application_info.set_client_info('Row ' || rw);

                  if self.s_rows(rw).cells(c) is null then
                     put_line('Loop "what_if > cell", row ' || rw || ', column ' || c);

                     dbms_application_info.set_client_info
                     ( 'Row ' || rw || ', cell ' || c || ', total guesses ' || v_guesses );

                     v_square# := which_square(rw,c);

                     $if $$extended_diagnostics $then
                        if p_verbose then
                           self.s_rows(rw).print('Row ' || rw, false);
                           -- self.s_columns(c).print('Column ' || c, false);
                           -- self.s_squares(v_square#).print('Square ' || v_square#, false);
                           declare
                              v_values sudoku_cell_tt := self.s_rows(rw).free_values;
                           begin
                              put('Free values for row ' || rw || ':     ');
                              for j in v_values.first..v_values.last loop
                                 begin
                                    put(v_values(j) || ' ');
                                 exception
                                    when no_data_found then null;
                                 end;
                              end loop;
                              put_line('');
                           end;
                           declare
                              v_values sudoku_cell_tt := self.s_columns(c).free_values;
                           begin
                              put('Free values for column ' || c || ':  ');
                              for j in v_values.first..v_values.last loop
                                 begin
                                    put(v_values(j) || ' ');
                                 exception
                                    when no_data_found then null;
                                 end;
                              end loop;
                              put_line('');
                           end;
                           declare
                              v_values sudoku_cell_tt := self.s_squares(v_square#).free_values;
                           begin
                              put('Free values for square ' || v_square# || ':  ');
                              for j in v_values.first..v_values.last loop
                                 begin
                                    put(v_values(j) || ' ');
                                 exception
                                    when no_data_found then null;
                                 end;
                              end loop;
                              put_line('');
                           end;
                        end if;
                     $end

                     v_candidates :=
                        intersection
                        ( self.s_rows(rw).free_values
                        , self.s_columns(c).free_values
                        , self.s_squares(v_square#).free_values );

                     v_candidate_count_orig := v_candidates.count;

                     if v_candidates.count = 0
                     then
                        -- Should never happen :)
                        -- Something must have gone wrong, because the intersection check above found no possible values
                        -- for this cell. Either the sudoku is invalid or we made a wrong guess earlier.
                        -- Revert to last stable version and quit this cell.
                        put_line('Abandoning guess ' || v_guesses || ' at row ' || rw || ', column ' || c || '. Restoring last stable version.');
                        self := v_last_stable_sudoku;
                        if p_verbose then
                           self.print();
                           new_line();
                        end if;

                        exit cell;
                     else
                        <<candidate>>
                        for i in v_candidates.first..v_candidates.last loop
                           exit when self.status = 'Solved';

                           put_line('Restoring last stable version');
                           -- self.s_rows := v_last_stable_sudoku.s_rows;
                           self := v_last_stable_sudoku;

                           if p_verbose then
                              self.print();
                           end if;

                           v_guesses := v_guesses +1;

                           -- Set current cell to the next possible value and retry solve_inner().

                           put ( chr(10) ||
                           'Guess ' || v_guesses ||
                           ': row ' || rw ||
                           ' col ' || c ||
                           ', trying ' || v_candidates(i) ||
                           ' (#' || (i + v_candidates.count - v_candidate_count_orig) ||  -- Adjust for changed count
                           ' of ' || v_candidates.count || ' possible values {');

                           -- Diagnostic message listing remaining candidates:
                           for j in v_candidates.first..v_candidates.last loop
                              begin
                                 put(v_candidates(j) || case j when v_candidates.last then '}' else ',' end);
                              exception
                                 when no_data_found then null;  -- May have been eliminated
                              end;
                           end loop;
                           put_line(' )');

                           self.s_rows(rw).set_cell(c,v_candidates(i));

                           -- We set a cell to one of its possible values - now see whether that helped:
                           begin
                              -- If the current test values are invalid, solve_inner() will fail and raise
                              -- exception sudoku_broken, and we undo it and continue with the next text value:
                              solve_inner(p_verbose);

                              -- If still unsolved after call to main solver, revert to state before current guess:
                              if self.status = 'Solved' then
                                 put_line
                                 ( chr(10) ||
                                 'Guess ' || v_guesses || ' successful' ||
                                 ' (value "' || v_candidates(i) ||
                                 '" in row ' || rw ||
                                 ', col ' || c || ')' );
                              end if;
                           exception
                              -- If the guess was wrong, some part of the sudoku will not add up,
                              -- so undo this change and continue on to next guess until one does not fail:
                              when sudoku_broken then
                                 -- If this guess resulted in an invalid sudoku, we can eliminate this candidate.

                                 put_line(sqlerrm);

                                 put_line
                                 ( 'Sudoku was invalid with row ' || rw || ', col ' || c ||
                                 ' = ' || v_candidates(i)  || ': eliminating as candidate.' );

                                 -- This candidate value cannot go in this cell. Maybe that only leaves one value?
                                 v_candidates.delete(i);

                                 if v_candidates.count = 1 then
                                    -- This is the only value left - assuming sudoku is valid, it must be correct:
                                    self := v_last_stable_sudoku;
                                    self.s_rows(rw).set_cell(c,v_candidates(v_candidates.first));
                                    v_last_stable_sudoku := self;

                                    put_line('Elimination only left value '|| v_candidates(v_candidates.first));

                                    -- self.s_rows(rw).print('Row ' || rw || ' now:', false);
                                    put_line('Saved this version');

                                    put('Guess: ');
                                    put_line('Row ' || rw || ' col ' || c || ' set to ' || v_candidates(v_candidates.first));

                                    -- Restart entire what-if cycle with updated sudoku:
                                    exit what_if;
                                 else
                                    put_line(v_candidates.count || ' candidates remain for row ' || rw || ', col ' || c);
                                 end if;
                           end;
                        end loop candidate;
                     end if;  -- if there are any candidates for cell c
                  end if; -- if the cell is null
               end loop cell;

            end loop what_if;
         end loop what_if_restarts;
      end;
   end solve;

   member function solved_cursor  -- Results as ref cursor
      return sys_refcursor
   is
      c_results sys_refcursor;
   begin
      open c_results for
         select min(decode(cell#,1,c)) as col1
             , min(decode(cell#,2,c)) as col2
             , min(decode(cell#,3,c)) as col3
             , min(decode(cell#,4,c)) as col4
             , min(decode(cell#,5,c)) as col5
             , min(decode(cell#,6,c)) as col6
             , min(decode(cell#,7,c)) as col7
             , min(decode(cell#,8,c)) as col8
             , min(decode(cell#,9,c)) as col9
         from
         ( -- 9i seems to need cast with union
           select 1 as row#, rownum as cell#, r.column_value as c
           from   table(cast(self.s_rows(1).cells as sudoku_cell_tt)) r
           union all
           select 2, rownum, r.column_value
           from   table(cast(self.s_rows(2).cells as sudoku_cell_tt)) r
           union all
           select 3, rownum, r.column_value
           from   table(cast(self.s_rows(3).cells as sudoku_cell_tt)) r
           union all
           select 4, rownum, r.column_value
           from   table(cast(self.s_rows(4).cells as sudoku_cell_tt)) r
           union all
           select 5, rownum, r.column_value
           from   table(cast(self.s_rows(5).cells as sudoku_cell_tt)) r
           union all
           select 6, rownum, r.column_value
           from   table(cast(self.s_rows(6).cells as sudoku_cell_tt)) r
           union all
           select 7, rownum, r.column_value
           from   table(cast(self.s_rows(7).cells as sudoku_cell_tt)) r
           union all
           select 8, rownum, r.column_value
           from   table(cast(self.s_rows(8).cells as sudoku_cell_tt)) r
           union all
           select 9, rownum, r.column_value
           from   table(cast(self.s_rows(9).cells as sudoku_cell_tt)) r
         )
         group by row# ;

      return c_results;
   end solved_cursor;


   constructor function sudoku
      ( p_row1 varchar2 default '         '
      , p_row2 varchar2 default '         '
      , p_row3 varchar2 default '         '
      , p_row4 varchar2 default '         '
      , p_row5 varchar2 default '         '
      , p_row6 varchar2 default '         '
      , p_row7 varchar2 default '         '
      , p_row8 varchar2 default '         '
      , p_row9 varchar2 default '         '
      , p_verbose boolean default false )
      return self as result
   is
   begin
      self.s_rows := new sudoku_element_tt();
      self.s_rows.extend(9);
      self.s_rows(1) := sudoku_element(p_row1);
      self.s_rows(2) := sudoku_element(p_row2);
      self.s_rows(3) := sudoku_element(p_row3);
      self.s_rows(4) := sudoku_element(p_row4);
      self.s_rows(5) := sudoku_element(p_row5);
      self.s_rows(6) := sudoku_element(p_row6);
      self.s_rows(7) := sudoku_element(p_row7);
      self.s_rows(8) := sudoku_element(p_row8);
      self.s_rows(9) := sudoku_element(p_row9);

      -- Invoke main constructor (diagnotic messages, solve() etc):
      self := new sudoku(self.s_rows, p_verbose);

      return;
   end sudoku;


   constructor function sudoku
      ( p_rows sudoku_element_tt
      , p_verbose boolean default false )
      return self as result
   is
   begin
      self.status := 'New';
      self.s_rows := p_rows;

      if p_verbose then
         $if $$extended_diagnostics $then
            dbms_output.put_line('Toggle extended diagnostics using conditional compilation:');
            dbms_output.put_line(q'|alter type sudoku compile body plsql_ccflags = 'extended_diagnostics:false' reuse settings;|');
            dbms_output.put_line('or at session level:');
            dbms_output.put_line(q'|alter session set plsql_ccflags = 'extended_diagnostics:false';|' || chr(10));
         $else
            dbms_output.put_line('Toggle extended diagnostics using conditional compilation:');
            dbms_output.put_line(q'|alter type sudoku compile body plsql_ccflags = 'extended_diagnostics:true' reuse settings;|');
            dbms_output.put_line('or at session level:');
            dbms_output.put_line(q'|alter session set plsql_ccflags = 'extended_diagnostics:true';|' || chr(10));
         $end
      end if;

      dbms_output.put_line('Before:');
      self.print();

      self.solve(p_verbose);

      dbms_output.put_line('After:');
      self.print();

      dbms_output.put_line(self.status);

      return;
   end sudoku;
end;
/

show errors type body sudoku

prompt "set serveroutput on size unlimited" requires sql*Plus 10g:
set serveroutput on size 1000000
set serveroutput on size unlimited
set timing on

exec  dbms_output.enable(null)

prompt
prompt db-innovations '#3: pencil mark cross-hatching' example
declare
   v_sudoku sudoku :=
      new sudoku
      ( sudoku_element_tt
        ( sudoku_element(' 7    25 ')
        , sudoku_element(' 8 792 6 ')
        , sudoku_element('  6 51  7')
        , sudoku_element('  31     ')
        , sudoku_element('  7 2 6  ')
        , sudoku_element('     48  ')
        , sudoku_element('7  6391  ')
        , sudoku_element('631548792')
        , sudoku_element('948217536') )
      , false
      );
begin
   null;
end;
/

declare
   v_row sudoku_element;
begin
   -- Test validation of a single element containing duplicates:
   v_row := new sudoku_element(1,2,3,4,4,6,7,8,9);
   v_row.check_valid;
   v_row.print;
end;
.

-- Default sudoku_element constructor syntax (new simple text input is available in version 4)
declare
   v_sudoku sudoku :=
      new sudoku
      ( sudoku_element_tt
        ( sudoku_element(null,null,null,6,   1,   3,   4,   null,null)
        , sudoku_element(8,   4,   null,null,2,   null,3,   null,null)
        , sudoku_element(5,   6,   null,null,null,null,9,   null,null)
        , sudoku_element(null,null,null,3,   null,null,8,   5,   9   )
        , sudoku_element(6,   null,null,1,   null,4,   null,null,3   )
        , sudoku_element(3,   7,   5,   null,null,2,   null,null,null)
        , sudoku_element(null,null,8,   null,null,null,null,7,   1   )
        , sudoku_element(null,null,6,   null,9,   null,null,3,   8   )
        , sudoku_element(null,null,2,   8,   3,   6,   null,null,null))
      );
begin
   null;
end;
.

declare
   v_sudoku sudoku :=
      new sudoku
      ( sudoku_element_tt
        ( sudoku_element(2,   5,   null,null,null,null,3,   null,null)
        , sudoku_element(null,3,   null,null,null,null,null,7,   null)
        , sudoku_element(4,   null,7,   null,null,6,   2,   null,null)
        , sudoku_element(1,   2,   8,   null,3,   null,null,null,6   )
        , sudoku_element(9,   6,   null,null,null,null,null,null,7   )
        , sudoku_element(null,null,null,null,8,   9,   null,4,   2   )
        , sudoku_element(null,null,9,   null,2,   1,   4,   null,null)
        , sudoku_element(5,   null,null,8,   null,3,   7,   9,   null)
        , sudoku_element(null,null,null,4,   null,null,null,null,null))
      );
begin
   null;
end;
.

declare
   v_sudoku sudoku :=
      new sudoku
      ( sudoku_element_tt
        ( sudoku_element(2,   null,null,null,1,   9,   null,4,   null)
        , sudoku_element(null,8,   null,null,null,null,1,   7,   null)
        , sudoku_element(null,7,   null,null,8,   6,   5,   null,null)
        , sudoku_element(1,   null,null,8,   null,2,   null,null,7   )
        , sudoku_element(5,   null,3,   null,null,null,6,   null,4   )
        , sudoku_element(8,   null,null,4,   null,3,   null,null,2   )
        , sudoku_element(null,null,8,   2,   7,   null,null,6,   null)
        , sudoku_element(null,5,   4,   null,null,null,null,9,   null)
        , sudoku_element(null,1,   null,9,   4,   null,null,null,8   ))
      );
begin
   null;
end;
.

set autoprint on
var results refcursor

prompt Test ref cursor output:
prompt (also simplified sudoku_element text input)
declare
   v_sudoku sudoku := new sudoku
   ( '8 9    6 '
   , '4   2 7  '
   , '   1 6 4 '
   , '  8  25  '
   , '93  1  82'
   , '  23  9  '
   , ' 1 7 5   '
   , '  6 3   7'
   , ' 8    1 6'
   , true );
begin
   :results := v_sudoku.solved_cursor;
end;
/

prompt Tony Andrews test case:
prompt http://tonyandrews.blogspot.com/2006/01/plsql-so-doku-solver.html
prompt (also simplified text input)

declare
   v_sudoku sudoku := new sudoku
   ( ' 6 1 4 5 '
   , '  83 56  '
   , '2       1'
   , '8  4 7  6'
   , '  6   3  '
   , '7  9 1  4'
   , '5       2'
   , '  72 69  '
   , ' 4 5 8 7 '
   , true );
begin
   null;
end;
/

prompt Test sql interface:

select new sudoku
       ( ' 6 1 4 5 '
       , '  83 56  '
       , '2       1'
       , '8  4 7  6'
       , '  6   3  '
       , '7  9 1  4'
       , '5       2'
       , '  72 69  '
       , ' 4 5 8 7 ') as new_sudoku
from dual;

-- pre-10g sql*Plus only (dbms_output processing used not to be invoked following sql calls):
exec null


prompt
prompt Wikipedia Sudoku example
prompt http://en.wikipedia.org/wiki/Sudoku
declare
   v_sudoku sudoku := new sudoku
   ( '53  7    '
   , '6  195   '
   , ' 98    6 '
   , '8   6   3'
   , '4  8 3  1'
   , '7   2   6'
   , ' 6    28 '
   , '   419  5'
   , '    8  79'
   , false  -- "verbose": set to true for diagnostic output messages
   );
begin
   null;
end;
/

prompt
prompt Bill Magee test:
prompt http://www.billmagee.co.uk/oracle/sudoku/index.html
declare
   -- Could not solve with mk2
   v_sudoku sudoku := new sudoku
   ( ' 9    15 '
   , '42      8'
   , '  1 9   2'
   , '  5 6  9 '
   , '   8 5   '
   , ' 7  3 5  '
   , '6   2 3  '
   , '3      64'
   , ' 52    1 '
   , true  -- "verbose": set to true for diagnostic output messages
   );
begin
   null;
end;
.

r

prompt
prompt From "Extra Challenging (Very Hard) Print and Solve Sudoku Puzzles 01-04"
prompt http://puzzles.about.com/library/sudoku/qprsudokux01.htm
declare
   v_sudoku sudoku := new sudoku
     ( ' 2      7'
     , ' 7   4 1 '
     , '9 5      '
     , ' 8 63   2'
     , '7       1'
     , '2   18 6 '
     , '      4 9'
     , ' 3 1   2 '
     , '4      8 '
   , false  -- "verbose": set to true for diagnostic output messages
   );
begin
   null;
end;
.

r

prompt
prompt Metro 2007-04-05, difficulty rating ***(--)
declare
   v_sudoku sudoku := new sudoku
     ( '    3   6'
     , ' 914     '
     , '      9  '
     , '   32 7  '
     , '5   7 8 9'
     , '48       '
     , '84     5 '
     , '     6 1 '
     , '1   87   '
   , false  -- "verbose": set to true for diagnostic output messages
   );
begin
   null;
end;
.

r


prompt One for Chris:
declare
        v_sudoku sudoku := new sudoku
        ( '  3   2  '
        , ' 175 286 '
        , '2  4 6  3'
        , '5   9   4'
        , ' 2     1 '
        , ' 8     7 '
        , '  9   5  '
        , '   163   '
        , '    7    '
        , false
        );
begin
        null;
end;
.
r

-- The Metro example below is particularly hard: also tested with Tony Andrews solution (could not solve either):
-- (See Tony's blog for cell_pkg etc)
begin
cell_pkg.solve( '   5 1 8 5   4  7  9   65    48   5   9   1   8   37    26   9  7  2 4 3 1 3     ');
end;
.
-- ...and the Bill Magee sudoku solver:
begin
   sudoku_pkg.solve
   ( '   5 1 8 '
   , '5   4  7 '
   , ' 9   65  '
   , '  48   5 '
   , '  9   1  '
   , ' 8   37  '
   , '  26   9 '
   , ' 7  2 4 3'
   , ' 1 3     ' );
end;
.
select c1,c2,c3,c4,c5,c6,c7,c8,c9 from sudoku_layup order by y


prompt
prompt Metro 2007-04-20
prompt (not solveable with mk3, solved with mk4)
declare
   v_sudoku sudoku := new sudoku
   ( '   5 1 8 '
   , '5   4  7 '
   , ' 9   65  '
   , '  48   5 '
   , '  9   1  '
   , ' 8   37  '
   , '  26   9 '
   , ' 7  2 4 3'
   , ' 1 3     '
   , false  -- "verbose": set to true for diagnostic output messages
   );
begin
   null;
end;
.

r

prompt
prompt "Tough Sudoku "
prompt http://sudoku.com.au/sudokutips.aspx?Go=H1-9-1999
prompt (not solveable with mk3, solved with mk4)
declare
   v_sudoku sudoku := new sudoku
   ( '  34     '
   , '    67  9'
   , '82       '
   , '3   9  8 '
   , '  1   6  '
   , ' 4  5   2'
   , '       61'
   , '6  57    '
   , '     38  '
   , false  -- "verbose": set to true for diagnostic output messages
   );
begin
   null;
end;
.

r

prompt Requires one "what-if" iteration in mk4:
prompt Row 1 col 1 = 1 (1 of 2 possible values {1,8})
declare
   v_sudoku sudoku :=
   new sudoku
   ( ' 7 29563 '
   , '    8 9  '
   , '6 9 4    '
   , '   5 94  ' 
   , '49     56'
   , '  1  4   '
   , '    523 8'
   , '  2 3    '
   , ' 3 9 1 2 '
   , true
   );
begin
   null;
end;
.
r

prompt
prompt db-innovations 'Times Grand Final Ultra-Fiendish' example
prompt (Cannot currently solve - not attempting)
declare
   v_sudoku sudoku := new sudoku
   ( ' 38     2'
   , '  4 2  5 '
   , '     619 '
   , '   9  2  '
   , '    8    '
   , '4 2  1   '
   , ' 8       '
   , ' 9  3 8  '
   , '1     92 '
   , false
   );
begin
   null;
end;
.

l

prompt
prompt Toggle extended diagnostics using conditional compilation:
prompt alter type sudoku compile body plsql_ccflags = 'extended_diagnostics:true' reuse settings;
prompt
prompt or at session level:
prompt alter session set plsql_ccflags = 'extended_diagnostics:true';
prompt

