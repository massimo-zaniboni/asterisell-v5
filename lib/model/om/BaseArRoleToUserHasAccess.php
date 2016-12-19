<?php


abstract class BaseArRoleToUserHasAccess extends BaseObject  implements Persistent {


	
	protected static $peer;


	
	protected $ar_role_id;


	
	protected $ar_user_has_access_id;

	
	protected $aArRole;

	
	protected $aArUserHasAccess;

	
	protected $alreadyInSave = false;

	
	protected $alreadyInValidation = false;

	
	public function getArRoleId()
	{

		return $this->ar_role_id;
	}

	
	public function getArUserHasAccessId()
	{

		return $this->ar_user_has_access_id;
	}

	
	public function setArRoleId($v)
	{

						if ($v !== null && !is_int($v) && is_numeric($v)) {
			$v = (int) $v;
		}

		if ($this->ar_role_id !== $v) {
			$this->ar_role_id = $v;
			$this->modifiedColumns[] = ArRoleToUserHasAccessPeer::AR_ROLE_ID;
		}

		if ($this->aArRole !== null && $this->aArRole->getId() !== $v) {
			$this->aArRole = null;
		}

	} 
	
	public function setArUserHasAccessId($v)
	{

						if ($v !== null && !is_int($v) && is_numeric($v)) {
			$v = (int) $v;
		}

		if ($this->ar_user_has_access_id !== $v) {
			$this->ar_user_has_access_id = $v;
			$this->modifiedColumns[] = ArRoleToUserHasAccessPeer::AR_USER_HAS_ACCESS_ID;
		}

		if ($this->aArUserHasAccess !== null && $this->aArUserHasAccess->getId() !== $v) {
			$this->aArUserHasAccess = null;
		}

	} 
	
	public function hydrate(ResultSet $rs, $startcol = 1)
	{
		try {

			$this->ar_role_id = $rs->getInt($startcol + 0);

			$this->ar_user_has_access_id = $rs->getInt($startcol + 1);

			$this->resetModified();

			$this->setNew(false);

						return $startcol + 2; 
		} catch (Exception $e) {
			throw new PropelException("Error populating ArRoleToUserHasAccess object", $e);
		}
	}

	
	public function delete($con = null)
	{
		if ($this->isDeleted()) {
			throw new PropelException("This object has already been deleted.");
		}

		if ($con === null) {
			$con = Propel::getConnection(ArRoleToUserHasAccessPeer::DATABASE_NAME);
		}

		try {
			$con->begin();
			ArRoleToUserHasAccessPeer::doDelete($this, $con);
			$this->setDeleted(true);
			$con->commit();
		} catch (PropelException $e) {
			$con->rollback();
			throw $e;
		}
	}

	
	public function save($con = null)
	{
		if ($this->isDeleted()) {
			throw new PropelException("You cannot save an object that has been deleted.");
		}

		if ($con === null) {
			$con = Propel::getConnection(ArRoleToUserHasAccessPeer::DATABASE_NAME);
		}

		try {
			$con->begin();
			$affectedRows = $this->doSave($con);
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollback();
			throw $e;
		}
	}

	
	protected function doSave($con)
	{
		$affectedRows = 0; 		if (!$this->alreadyInSave) {
			$this->alreadyInSave = true;


												
			if ($this->aArRole !== null) {
				if ($this->aArRole->isModified()) {
					$affectedRows += $this->aArRole->save($con);
				}
				$this->setArRole($this->aArRole);
			}

			if ($this->aArUserHasAccess !== null) {
				if ($this->aArUserHasAccess->isModified()) {
					$affectedRows += $this->aArUserHasAccess->save($con);
				}
				$this->setArUserHasAccess($this->aArUserHasAccess);
			}


						if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArRoleToUserHasAccessPeer::doInsert($this, $con);
					$affectedRows += 1; 										 										 
					$this->setNew(false);
				} else {
					$affectedRows += ArRoleToUserHasAccessPeer::doUpdate($this, $con);
				}
				$this->resetModified(); 			}

			$this->alreadyInSave = false;
		}
		return $affectedRows;
	} 
	
	protected $validationFailures = array();

	
	public function getValidationFailures()
	{
		return $this->validationFailures;
	}

	
	public function validate($columns = null)
	{
		$res = $this->doValidate($columns);
		if ($res === true) {
			$this->validationFailures = array();
			return true;
		} else {
			$this->validationFailures = $res;
			return false;
		}
	}

	
	protected function doValidate($columns = null)
	{
		if (!$this->alreadyInValidation) {
			$this->alreadyInValidation = true;
			$retval = null;

			$failureMap = array();


												
			if ($this->aArRole !== null) {
				if (!$this->aArRole->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArRole->getValidationFailures());
				}
			}

			if ($this->aArUserHasAccess !== null) {
				if (!$this->aArUserHasAccess->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArUserHasAccess->getValidationFailures());
				}
			}


			if (($retval = ArRoleToUserHasAccessPeer::doValidate($this, $columns)) !== true) {
				$failureMap = array_merge($failureMap, $retval);
			}



			$this->alreadyInValidation = false;
		}

		return (!empty($failureMap) ? $failureMap : true);
	}

	
	public function getByName($name, $type = BasePeer::TYPE_PHPNAME)
	{
		$pos = ArRoleToUserHasAccessPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
		return $this->getByPosition($pos);
	}

	
	public function getByPosition($pos)
	{
		switch($pos) {
			case 0:
				return $this->getArRoleId();
				break;
			case 1:
				return $this->getArUserHasAccessId();
				break;
			default:
				return null;
				break;
		} 	}

	
	public function toArray($keyType = BasePeer::TYPE_PHPNAME)
	{
		$keys = ArRoleToUserHasAccessPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getArRoleId(),
			$keys[1] => $this->getArUserHasAccessId(),
		);
		return $result;
	}

	
	public function setByName($name, $value, $type = BasePeer::TYPE_PHPNAME)
	{
		$pos = ArRoleToUserHasAccessPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
		return $this->setByPosition($pos, $value);
	}

	
	public function setByPosition($pos, $value)
	{
		switch($pos) {
			case 0:
				$this->setArRoleId($value);
				break;
			case 1:
				$this->setArUserHasAccessId($value);
				break;
		} 	}

	
	public function fromArray($arr, $keyType = BasePeer::TYPE_PHPNAME)
	{
		$keys = ArRoleToUserHasAccessPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setArRoleId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setArUserHasAccessId($arr[$keys[1]]);
	}

	
	public function buildCriteria()
	{
		$criteria = new Criteria(ArRoleToUserHasAccessPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArRoleToUserHasAccessPeer::AR_ROLE_ID)) $criteria->add(ArRoleToUserHasAccessPeer::AR_ROLE_ID, $this->ar_role_id);
		if ($this->isColumnModified(ArRoleToUserHasAccessPeer::AR_USER_HAS_ACCESS_ID)) $criteria->add(ArRoleToUserHasAccessPeer::AR_USER_HAS_ACCESS_ID, $this->ar_user_has_access_id);

		return $criteria;
	}

	
	public function buildPkeyCriteria()
	{
		$criteria = new Criteria(ArRoleToUserHasAccessPeer::DATABASE_NAME);

		$criteria->add(ArRoleToUserHasAccessPeer::AR_ROLE_ID, $this->ar_role_id);
		$criteria->add(ArRoleToUserHasAccessPeer::AR_USER_HAS_ACCESS_ID, $this->ar_user_has_access_id);

		return $criteria;
	}

	
	public function getPrimaryKey()
	{
		$pks = array();

		$pks[0] = $this->getArRoleId();

		$pks[1] = $this->getArUserHasAccessId();

		return $pks;
	}

	
	public function setPrimaryKey($keys)
	{

		$this->setArRoleId($keys[0]);

		$this->setArUserHasAccessId($keys[1]);

	}

	
	public function copyInto($copyObj, $deepCopy = false)
	{


		$copyObj->setNew(true);

		$copyObj->setArRoleId(NULL); 
		$copyObj->setArUserHasAccessId(NULL); 
	}

	
	public function copy($deepCopy = false)
	{
				$clazz = get_class($this);
		$copyObj = new $clazz();
		$this->copyInto($copyObj, $deepCopy);
		return $copyObj;
	}

	
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArRoleToUserHasAccessPeer();
		}
		return self::$peer;
	}

	
	public function setArRole($v)
	{


		if ($v === null) {
			$this->setArRoleId(NULL);
		} else {
			$this->setArRoleId($v->getId());
		}


		$this->aArRole = $v;
	}


	
	public function getArRole($con = null)
	{
		if ($this->aArRole === null && ($this->ar_role_id !== null)) {
						include_once 'lib/model/om/BaseArRolePeer.php';

			$this->aArRole = ArRolePeer::retrieveByPK($this->ar_role_id, $con);

			
		}
		return $this->aArRole;
	}

	
	public function setArUserHasAccess($v)
	{


		if ($v === null) {
			$this->setArUserHasAccessId(NULL);
		} else {
			$this->setArUserHasAccessId($v->getId());
		}


		$this->aArUserHasAccess = $v;
	}


	
	public function getArUserHasAccess($con = null)
	{
		if ($this->aArUserHasAccess === null && ($this->ar_user_has_access_id !== null)) {
						include_once 'lib/model/om/BaseArUserHasAccessPeer.php';

			$this->aArUserHasAccess = ArUserHasAccessPeer::retrieveByPK($this->ar_user_has_access_id, $con);

			
		}
		return $this->aArUserHasAccess;
	}

} 