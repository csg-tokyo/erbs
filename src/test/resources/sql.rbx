Operator(origin)
  defs SELECT column FROM table (column: origin, table: origin)
    puts("access " + table + " and get " + column)
  end
end

SELECT 'name' FROM 'users'