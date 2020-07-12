-- Report the population of [column] within [table]
-- William Robertson - www.williamrobertson.net
--
-- e.g:
-- @ percent adr_post_code addresses
--
--    TOTAL  PRESENT  PERCENT  MISSING  PERCENT
-- -------- -------- -------- -------- --------
--   173278   124545    71.88    48733    28.12
--
-- Tip:
-- To add SQL conditions, include them within the TABLE parameter using double quotes
-- e.g:
-- @ percent adr_post_code "addresses where adr_type = 'B'"

col percent format 990.99 head "Percent"

cl comp
def column = "&1"
def table = "&2"
set numwidth 8 ver off

select count(*) "Total"
     , count(&column) "Present"
     , round( count(&column) * 100 / greatest(count(*),1), 2) percent
     , count(*) - count(&column) "Missing"
     , round( (count(*)-count(&column)) * 100 / greatest(count(*),1), 2) percent
from   &table
/
