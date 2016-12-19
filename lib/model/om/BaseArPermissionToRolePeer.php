<?php


abstract class BaseArPermissionToRolePeer {

	
	const DATABASE_NAME = 'propel';

	
	const TABLE_NAME = 'ar_permission_to_role';

	
	const CLASS_DEFAULT = 'lib.model.ArPermissionToRole';

	
	const NUM_COLUMNS = 2;

	
	const NUM_LAZY_LOAD_COLUMNS = 0;


	
	const AR_PERMISSION_ID = 'ar_permission_to_role.AR_PERMISSION_ID';

	
	const AR_ROLE_ID = 'ar_permission_to_role.AR_ROLE_ID';

	
	private static $phpNameMap = null;


	
	private static $fieldNames = array (
		BasePeer::TYPE_PHPNAME => array ('ArPermissionId', 'ArRoleId', ),
		BasePeer::TYPE_COLNAME => array (ArPermissionToRolePeer::AR_PERMISSION_ID, ArPermissionToRolePeer::AR_ROLE_ID, ),
		BasePeer::TYPE_FIELDNAME => array ('ar_permission_id', 'ar_role_id', ),
		BasePeer::TYPE_NUM => array (0, 1, )
	);

	
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('ArPermissionId' => 0, 'ArRoleId' => 1, ),
		BasePeer::TYPE_COLNAME => array (ArPermissionToRolePeer::AR_PERMISSION_ID => 0, ArPermissionToRolePeer::AR_ROLE_ID => 1, ),
		BasePeer::TYPE_FIELDNAME => array ('ar_permission_id' => 0, 'ar_role_id' => 1, ),
		BasePeer::TYPE_NUM => array (0, 1, )
	);

	
	public static function getMapBuilder()
	{
		include_once 'lib/model/map/ArPermissionToRoleMapBuilder.php';
		return BasePeer::getMapBuilder('lib.model.map.ArPermissionToRoleMapBuilder');
	}
	
	public static function getPhpNameMap()
	{
		if (self::$phpNameMap === null) {
			$map = ArPermissionToRolePeer::getTableMap();
			$columns = $map->getColumns();
			$nameMap = array();
			foreach ($columns as $column) {
				$nameMap[$column->getPhpName()] = $column->getColumnName();
			}
			self::$phpNameMap = $nameMap;
		}
		return self::$phpNameMap;
	}
	
	static public function translateFieldName($name, $fromType, $toType)
	{
		$toNames = self::getFieldNames($toType);
		$key = isset(self::$fieldKeys[$fromType][$name]) ? self::$fieldKeys[$fromType][$name] : null;
		if ($key === null) {
			throw new PropelException("'$name' could not be found in the field names of type '$fromType'. These are: " . print_r(self::$fieldKeys[$fromType], true));
		}
		return $toNames[$key];
	}

	

	static public function getFieldNames($type = BasePeer::TYPE_PHPNAME)
	{
		if (!array_key_exists($type, self::$fieldNames)) {
			throw new PropelException('Method getFieldNames() expects the parameter $type to be one of the class constants TYPE_PHPNAME, TYPE_COLNAME, TYPE_FIELDNAME, TYPE_NUM. ' . $type . ' was given.');
		}
		return self::$fieldNames[$type];
	}

	
	public static function alias($alias, $column)
	{
		return str_replace(ArPermissionToRolePeer::TABLE_NAME.'.', $alias.'.', $column);
	}

	
	public static function addSelectColumns(Criteria $criteria)
	{

		$criteria->addSelectColumn(ArPermissionToRolePeer::AR_PERMISSION_ID);

		$criteria->addSelectColumn(ArPermissionToRolePeer::AR_ROLE_ID);

	}

	const COUNT = 'COUNT(ar_permission_to_role.AR_PERMISSION_ID)';
	const COUNT_DISTINCT = 'COUNT(DISTINCT ar_permission_to_role.AR_PERMISSION_ID)';

	
	public static function doCount(Criteria $criteria, $distinct = false, $con = null)
	{
				$criteria = clone $criteria;

				$criteria->clearSelectColumns()->clearOrderByColumns();
		if ($distinct || in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->addSelectColumn(ArPermissionToRolePeer::COUNT_DISTINCT);
		} else {
			$criteria->addSelectColumn(ArPermissionToRolePeer::COUNT);
		}

				foreach($criteria->getGroupByColumns() as $column)
		{
			$criteria->addSelectColumn($column);
		}

		$rs = ArPermissionToRolePeer::doSelectRS($criteria, $con);
		if ($rs->next()) {
			return $rs->getInt(1);
		} else {
						return 0;
		}
	}
	
	public static function doSelectOne(Criteria $criteria, $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArPermissionToRolePeer::doSelect($critcopy, $con);
		if ($objects) {
			return $objects[0];
		}
		return null;
	}
	
	public static function doSelect(Criteria $criteria, $con = null)
	{
		return ArPermissionToRolePeer::populateObjects(ArPermissionToRolePeer::doSelectRS($criteria, $con));
	}
	
	public static function doSelectRS(Criteria $criteria, $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(self::DATABASE_NAME);
		}

		if (!$criteria->getSelectColumns()) {
			$criteria = clone $criteria;
			ArPermissionToRolePeer::addSelectColumns($criteria);
		}

				$criteria->setDbName(self::DATABASE_NAME);

						return BasePeer::doSelect($criteria, $con);
	}
	
	public static function populateObjects(ResultSet $rs)
	{
		$results = array();
	
				$cls = ArPermissionToRolePeer::getOMClass();
		$cls = Propel::import($cls);
				while($rs->next()) {
		
			$obj = new $cls();
			$obj->hydrate($rs);
			$results[] = $obj;
			
		}
		return $results;
	}

	
	public static function doCountJoinArPermission(Criteria $criteria, $distinct = false, $con = null)
	{
				$criteria = clone $criteria;

				$criteria->clearSelectColumns()->clearOrderByColumns();
		if ($distinct || in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->addSelectColumn(ArPermissionToRolePeer::COUNT_DISTINCT);
		} else {
			$criteria->addSelectColumn(ArPermissionToRolePeer::COUNT);
		}

				foreach($criteria->getGroupByColumns() as $column)
		{
			$criteria->addSelectColumn($column);
		}

		$criteria->addJoin(ArPermissionToRolePeer::AR_PERMISSION_ID, ArPermissionPeer::ID);

		$rs = ArPermissionToRolePeer::doSelectRS($criteria, $con);
		if ($rs->next()) {
			return $rs->getInt(1);
		} else {
						return 0;
		}
	}


	
	public static function doCountJoinArRole(Criteria $criteria, $distinct = false, $con = null)
	{
				$criteria = clone $criteria;

				$criteria->clearSelectColumns()->clearOrderByColumns();
		if ($distinct || in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->addSelectColumn(ArPermissionToRolePeer::COUNT_DISTINCT);
		} else {
			$criteria->addSelectColumn(ArPermissionToRolePeer::COUNT);
		}

				foreach($criteria->getGroupByColumns() as $column)
		{
			$criteria->addSelectColumn($column);
		}

		$criteria->addJoin(ArPermissionToRolePeer::AR_ROLE_ID, ArRolePeer::ID);

		$rs = ArPermissionToRolePeer::doSelectRS($criteria, $con);
		if ($rs->next()) {
			return $rs->getInt(1);
		} else {
						return 0;
		}
	}


	
	public static function doSelectJoinArPermission(Criteria $c, $con = null)
	{
		$c = clone $c;

				if ($c->getDbName() == Propel::getDefaultDB()) {
			$c->setDbName(self::DATABASE_NAME);
		}

		ArPermissionToRolePeer::addSelectColumns($c);
		$startcol = (ArPermissionToRolePeer::NUM_COLUMNS - ArPermissionToRolePeer::NUM_LAZY_LOAD_COLUMNS) + 1;
		ArPermissionPeer::addSelectColumns($c);

		$c->addJoin(ArPermissionToRolePeer::AR_PERMISSION_ID, ArPermissionPeer::ID);
		$rs = BasePeer::doSelect($c, $con);
		$results = array();

		while($rs->next()) {

			$omClass = ArPermissionToRolePeer::getOMClass();

			$cls = Propel::import($omClass);
			$obj1 = new $cls();
			$obj1->hydrate($rs);

			$omClass = ArPermissionPeer::getOMClass();

			$cls = Propel::import($omClass);
			$obj2 = new $cls();
			$obj2->hydrate($rs, $startcol);

			$newObject = true;
			foreach($results as $temp_obj1) {
				$temp_obj2 = $temp_obj1->getArPermission(); 				if ($temp_obj2->getPrimaryKey() === $obj2->getPrimaryKey()) {
					$newObject = false;
										$temp_obj2->addArPermissionToRole($obj1); 					break;
				}
			}
			if ($newObject) {
				$obj2->initArPermissionToRoles();
				$obj2->addArPermissionToRole($obj1); 			}
			$results[] = $obj1;
		}
		return $results;
	}


	
	public static function doSelectJoinArRole(Criteria $c, $con = null)
	{
		$c = clone $c;

				if ($c->getDbName() == Propel::getDefaultDB()) {
			$c->setDbName(self::DATABASE_NAME);
		}

		ArPermissionToRolePeer::addSelectColumns($c);
		$startcol = (ArPermissionToRolePeer::NUM_COLUMNS - ArPermissionToRolePeer::NUM_LAZY_LOAD_COLUMNS) + 1;
		ArRolePeer::addSelectColumns($c);

		$c->addJoin(ArPermissionToRolePeer::AR_ROLE_ID, ArRolePeer::ID);
		$rs = BasePeer::doSelect($c, $con);
		$results = array();

		while($rs->next()) {

			$omClass = ArPermissionToRolePeer::getOMClass();

			$cls = Propel::import($omClass);
			$obj1 = new $cls();
			$obj1->hydrate($rs);

			$omClass = ArRolePeer::getOMClass();

			$cls = Propel::import($omClass);
			$obj2 = new $cls();
			$obj2->hydrate($rs, $startcol);

			$newObject = true;
			foreach($results as $temp_obj1) {
				$temp_obj2 = $temp_obj1->getArRole(); 				if ($temp_obj2->getPrimaryKey() === $obj2->getPrimaryKey()) {
					$newObject = false;
										$temp_obj2->addArPermissionToRole($obj1); 					break;
				}
			}
			if ($newObject) {
				$obj2->initArPermissionToRoles();
				$obj2->addArPermissionToRole($obj1); 			}
			$results[] = $obj1;
		}
		return $results;
	}


	
	public static function doCountJoinAll(Criteria $criteria, $distinct = false, $con = null)
	{
		$criteria = clone $criteria;

				$criteria->clearSelectColumns()->clearOrderByColumns();
		if ($distinct || in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->addSelectColumn(ArPermissionToRolePeer::COUNT_DISTINCT);
		} else {
			$criteria->addSelectColumn(ArPermissionToRolePeer::COUNT);
		}

				foreach($criteria->getGroupByColumns() as $column)
		{
			$criteria->addSelectColumn($column);
		}

		$criteria->addJoin(ArPermissionToRolePeer::AR_PERMISSION_ID, ArPermissionPeer::ID);

		$criteria->addJoin(ArPermissionToRolePeer::AR_ROLE_ID, ArRolePeer::ID);

		$rs = ArPermissionToRolePeer::doSelectRS($criteria, $con);
		if ($rs->next()) {
			return $rs->getInt(1);
		} else {
						return 0;
		}
	}


	
	public static function doSelectJoinAll(Criteria $c, $con = null)
	{
		$c = clone $c;

				if ($c->getDbName() == Propel::getDefaultDB()) {
			$c->setDbName(self::DATABASE_NAME);
		}

		ArPermissionToRolePeer::addSelectColumns($c);
		$startcol2 = (ArPermissionToRolePeer::NUM_COLUMNS - ArPermissionToRolePeer::NUM_LAZY_LOAD_COLUMNS) + 1;

		ArPermissionPeer::addSelectColumns($c);
		$startcol3 = $startcol2 + ArPermissionPeer::NUM_COLUMNS;

		ArRolePeer::addSelectColumns($c);
		$startcol4 = $startcol3 + ArRolePeer::NUM_COLUMNS;

		$c->addJoin(ArPermissionToRolePeer::AR_PERMISSION_ID, ArPermissionPeer::ID);

		$c->addJoin(ArPermissionToRolePeer::AR_ROLE_ID, ArRolePeer::ID);

		$rs = BasePeer::doSelect($c, $con);
		$results = array();

		while($rs->next()) {

			$omClass = ArPermissionToRolePeer::getOMClass();


			$cls = Propel::import($omClass);
			$obj1 = new $cls();
			$obj1->hydrate($rs);


					
			$omClass = ArPermissionPeer::getOMClass();


			$cls = Propel::import($omClass);
			$obj2 = new $cls();
			$obj2->hydrate($rs, $startcol2);

			$newObject = true;
			for ($j=0, $resCount=count($results); $j < $resCount; $j++) {
				$temp_obj1 = $results[$j];
				$temp_obj2 = $temp_obj1->getArPermission(); 				if ($temp_obj2->getPrimaryKey() === $obj2->getPrimaryKey()) {
					$newObject = false;
					$temp_obj2->addArPermissionToRole($obj1); 					break;
				}
			}

			if ($newObject) {
				$obj2->initArPermissionToRoles();
				$obj2->addArPermissionToRole($obj1);
			}


					
			$omClass = ArRolePeer::getOMClass();


			$cls = Propel::import($omClass);
			$obj3 = new $cls();
			$obj3->hydrate($rs, $startcol3);

			$newObject = true;
			for ($j=0, $resCount=count($results); $j < $resCount; $j++) {
				$temp_obj1 = $results[$j];
				$temp_obj3 = $temp_obj1->getArRole(); 				if ($temp_obj3->getPrimaryKey() === $obj3->getPrimaryKey()) {
					$newObject = false;
					$temp_obj3->addArPermissionToRole($obj1); 					break;
				}
			}

			if ($newObject) {
				$obj3->initArPermissionToRoles();
				$obj3->addArPermissionToRole($obj1);
			}

			$results[] = $obj1;
		}
		return $results;
	}


	
	public static function doCountJoinAllExceptArPermission(Criteria $criteria, $distinct = false, $con = null)
	{
				$criteria = clone $criteria;

				$criteria->clearSelectColumns()->clearOrderByColumns();
		if ($distinct || in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->addSelectColumn(ArPermissionToRolePeer::COUNT_DISTINCT);
		} else {
			$criteria->addSelectColumn(ArPermissionToRolePeer::COUNT);
		}

				foreach($criteria->getGroupByColumns() as $column)
		{
			$criteria->addSelectColumn($column);
		}

		$criteria->addJoin(ArPermissionToRolePeer::AR_ROLE_ID, ArRolePeer::ID);

		$rs = ArPermissionToRolePeer::doSelectRS($criteria, $con);
		if ($rs->next()) {
			return $rs->getInt(1);
		} else {
						return 0;
		}
	}


	
	public static function doCountJoinAllExceptArRole(Criteria $criteria, $distinct = false, $con = null)
	{
				$criteria = clone $criteria;

				$criteria->clearSelectColumns()->clearOrderByColumns();
		if ($distinct || in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->addSelectColumn(ArPermissionToRolePeer::COUNT_DISTINCT);
		} else {
			$criteria->addSelectColumn(ArPermissionToRolePeer::COUNT);
		}

				foreach($criteria->getGroupByColumns() as $column)
		{
			$criteria->addSelectColumn($column);
		}

		$criteria->addJoin(ArPermissionToRolePeer::AR_PERMISSION_ID, ArPermissionPeer::ID);

		$rs = ArPermissionToRolePeer::doSelectRS($criteria, $con);
		if ($rs->next()) {
			return $rs->getInt(1);
		} else {
						return 0;
		}
	}


	
	public static function doSelectJoinAllExceptArPermission(Criteria $c, $con = null)
	{
		$c = clone $c;

								if ($c->getDbName() == Propel::getDefaultDB()) {
			$c->setDbName(self::DATABASE_NAME);
		}

		ArPermissionToRolePeer::addSelectColumns($c);
		$startcol2 = (ArPermissionToRolePeer::NUM_COLUMNS - ArPermissionToRolePeer::NUM_LAZY_LOAD_COLUMNS) + 1;

		ArRolePeer::addSelectColumns($c);
		$startcol3 = $startcol2 + ArRolePeer::NUM_COLUMNS;

		$c->addJoin(ArPermissionToRolePeer::AR_ROLE_ID, ArRolePeer::ID);


		$rs = BasePeer::doSelect($c, $con);
		$results = array();

		while($rs->next()) {

			$omClass = ArPermissionToRolePeer::getOMClass();

			$cls = Propel::import($omClass);
			$obj1 = new $cls();
			$obj1->hydrate($rs);

			$omClass = ArRolePeer::getOMClass();


			$cls = Propel::import($omClass);
			$obj2  = new $cls();
			$obj2->hydrate($rs, $startcol2);

			$newObject = true;
			for ($j=0, $resCount=count($results); $j < $resCount; $j++) {
				$temp_obj1 = $results[$j];
				$temp_obj2 = $temp_obj1->getArRole(); 				if ($temp_obj2->getPrimaryKey() === $obj2->getPrimaryKey()) {
					$newObject = false;
					$temp_obj2->addArPermissionToRole($obj1);
					break;
				}
			}

			if ($newObject) {
				$obj2->initArPermissionToRoles();
				$obj2->addArPermissionToRole($obj1);
			}

			$results[] = $obj1;
		}
		return $results;
	}


	
	public static function doSelectJoinAllExceptArRole(Criteria $c, $con = null)
	{
		$c = clone $c;

								if ($c->getDbName() == Propel::getDefaultDB()) {
			$c->setDbName(self::DATABASE_NAME);
		}

		ArPermissionToRolePeer::addSelectColumns($c);
		$startcol2 = (ArPermissionToRolePeer::NUM_COLUMNS - ArPermissionToRolePeer::NUM_LAZY_LOAD_COLUMNS) + 1;

		ArPermissionPeer::addSelectColumns($c);
		$startcol3 = $startcol2 + ArPermissionPeer::NUM_COLUMNS;

		$c->addJoin(ArPermissionToRolePeer::AR_PERMISSION_ID, ArPermissionPeer::ID);


		$rs = BasePeer::doSelect($c, $con);
		$results = array();

		while($rs->next()) {

			$omClass = ArPermissionToRolePeer::getOMClass();

			$cls = Propel::import($omClass);
			$obj1 = new $cls();
			$obj1->hydrate($rs);

			$omClass = ArPermissionPeer::getOMClass();


			$cls = Propel::import($omClass);
			$obj2  = new $cls();
			$obj2->hydrate($rs, $startcol2);

			$newObject = true;
			for ($j=0, $resCount=count($results); $j < $resCount; $j++) {
				$temp_obj1 = $results[$j];
				$temp_obj2 = $temp_obj1->getArPermission(); 				if ($temp_obj2->getPrimaryKey() === $obj2->getPrimaryKey()) {
					$newObject = false;
					$temp_obj2->addArPermissionToRole($obj1);
					break;
				}
			}

			if ($newObject) {
				$obj2->initArPermissionToRoles();
				$obj2->addArPermissionToRole($obj1);
			}

			$results[] = $obj1;
		}
		return $results;
	}

	
	public static function getTableMap()
	{
		return Propel::getDatabaseMap(self::DATABASE_NAME)->getTable(self::TABLE_NAME);
	}

	
	public static function getOMClass()
	{
		return ArPermissionToRolePeer::CLASS_DEFAULT;
	}

	
	public static function doInsert($values, $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(self::DATABASE_NAME);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; 		} else {
			$criteria = $values->buildCriteria(); 		}


				$criteria->setDbName(self::DATABASE_NAME);

		try {
									$con->begin();
			$pk = BasePeer::doInsert($criteria, $con);
			$con->commit();
		} catch(PropelException $e) {
			$con->rollback();
			throw $e;
		}

		return $pk;
	}

	
	public static function doUpdate($values, $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(self::DATABASE_NAME);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; 
			$comparison = $criteria->getComparison(ArPermissionToRolePeer::AR_PERMISSION_ID);
			$selectCriteria->add(ArPermissionToRolePeer::AR_PERMISSION_ID, $criteria->remove(ArPermissionToRolePeer::AR_PERMISSION_ID), $comparison);

			$comparison = $criteria->getComparison(ArPermissionToRolePeer::AR_ROLE_ID);
			$selectCriteria->add(ArPermissionToRolePeer::AR_ROLE_ID, $criteria->remove(ArPermissionToRolePeer::AR_ROLE_ID), $comparison);

		} else { 			$criteria = $values->buildCriteria(); 			$selectCriteria = $values->buildPkeyCriteria(); 		}

				$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(self::DATABASE_NAME);
		}
		$affectedRows = 0; 		try {
									$con->begin();
			$affectedRows += BasePeer::doDeleteAll(ArPermissionToRolePeer::TABLE_NAME, $con);
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollback();
			throw $e;
		}
	}

	
	 public static function doDelete($values, $con = null)
	 {
		if ($con === null) {
			$con = Propel::getConnection(ArPermissionToRolePeer::DATABASE_NAME);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; 		} elseif ($values instanceof ArPermissionToRole) {

			$criteria = $values->buildPkeyCriteria();
		} else {
						$criteria = new Criteria(self::DATABASE_NAME);
												if(count($values) == count($values, COUNT_RECURSIVE))
			{
								$values = array($values);
			}
			$vals = array();
			foreach($values as $value)
			{

				$vals[0][] = $value[0];
				$vals[1][] = $value[1];
			}

			$criteria->add(ArPermissionToRolePeer::AR_PERMISSION_ID, $vals[0], Criteria::IN);
			$criteria->add(ArPermissionToRolePeer::AR_ROLE_ID, $vals[1], Criteria::IN);
		}

				$criteria->setDbName(self::DATABASE_NAME);

		$affectedRows = 0; 
		try {
									$con->begin();
			
			$affectedRows += BasePeer::doDelete($criteria, $con);
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollback();
			throw $e;
		}
	}

	
	public static function doValidate(ArPermissionToRole $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArPermissionToRolePeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArPermissionToRolePeer::TABLE_NAME);

			if (! is_array($cols)) {
				$cols = array($cols);
			}

			foreach($cols as $colName) {
				if ($tableMap->containsColumn($colName)) {
					$get = 'get' . $tableMap->getColumn($colName)->getPhpName();
					$columns[$colName] = $obj->$get();
				}
			}
		} else {

		}

		$res =  BasePeer::doValidate(ArPermissionToRolePeer::DATABASE_NAME, ArPermissionToRolePeer::TABLE_NAME, $columns);
    if ($res !== true) {
        $request = sfContext::getInstance()->getRequest();
        foreach ($res as $failed) {
            $col = ArPermissionToRolePeer::translateFieldname($failed->getColumn(), BasePeer::TYPE_COLNAME, BasePeer::TYPE_PHPNAME);
            $request->setError($col, $failed->getMessage());
        }
    }

    return $res;
	}

	
	public static function retrieveByPK( $ar_permission_id, $ar_role_id, $con = null) {
		if ($con === null) {
			$con = Propel::getConnection(self::DATABASE_NAME);
		}
		$criteria = new Criteria();
		$criteria->add(ArPermissionToRolePeer::AR_PERMISSION_ID, $ar_permission_id);
		$criteria->add(ArPermissionToRolePeer::AR_ROLE_ID, $ar_role_id);
		$v = ArPermissionToRolePeer::doSelect($criteria, $con);

		return !empty($v) ? $v[0] : null;
	}
} 
if (Propel::isInit()) {
			try {
		BaseArPermissionToRolePeer::getMapBuilder();
	} catch (Exception $e) {
		Propel::log('Could not initialize Peer: ' . $e->getMessage(), Propel::LOG_ERR);
	}
} else {
			require_once 'lib/model/map/ArPermissionToRoleMapBuilder.php';
	Propel::registerMapBuilder('lib.model.map.ArPermissionToRoleMapBuilder');
}
