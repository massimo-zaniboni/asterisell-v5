<?php


abstract class BaseArPermissionToUserHasAccess extends BaseObject  implements Persistent {


	
	protected static $peer;


	
	protected $ar_permission_id;


	
	protected $ar_user_has_access_id;

	
	protected $aArPermission;

	
	protected $aArUserHasAccess;

	
	protected $alreadyInSave = false;

	
	protected $alreadyInValidation = false;

	
	public function getArPermissionId()
	{

		return $this->ar_permission_id;
	}

	
	public function getArUserHasAccessId()
	{

		return $this->ar_user_has_access_id;
	}

	
	public function setArPermissionId($v)
	{

						if ($v !== null && !is_int($v) && is_numeric($v)) {
			$v = (int) $v;
		}

		if ($this->ar_permission_id !== $v) {
			$this->ar_permission_id = $v;
			$this->modifiedColumns[] = ArPermissionToUserHasAccessPeer::AR_PERMISSION_ID;
		}

		if ($this->aArPermission !== null && $this->aArPermission->getId() !== $v) {
			$this->aArPermission = null;
		}

	} 
	
	public function setArUserHasAccessId($v)
	{

						if ($v !== null && !is_int($v) && is_numeric($v)) {
			$v = (int) $v;
		}

		if ($this->ar_user_has_access_id !== $v) {
			$this->ar_user_has_access_id = $v;
			$this->modifiedColumns[] = ArPermissionToUserHasAccessPeer::AR_USER_HAS_ACCESS_ID;
		}

		if ($this->aArUserHasAccess !== null && $this->aArUserHasAccess->getId() !== $v) {
			$this->aArUserHasAccess = null;
		}

	} 
	
	public function hydrate(ResultSet $rs, $startcol = 1)
	{
		try {

			$this->ar_permission_id = $rs->getInt($startcol + 0);

			$this->ar_user_has_access_id = $rs->getInt($startcol + 1);

			$this->resetModified();

			$this->setNew(false);

						return $startcol + 2; 
		} catch (Exception $e) {
			throw new PropelException("Error populating ArPermissionToUserHasAccess object", $e);
		}
	}

	
	public function delete($con = null)
	{
		if ($this->isDeleted()) {
			throw new PropelException("This object has already been deleted.");
		}

		if ($con === null) {
			$con = Propel::getConnection(ArPermissionToUserHasAccessPeer::DATABASE_NAME);
		}

		try {
			$con->begin();
			ArPermissionToUserHasAccessPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArPermissionToUserHasAccessPeer::DATABASE_NAME);
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


												
			if ($this->aArPermission !== null) {
				if ($this->aArPermission->isModified()) {
					$affectedRows += $this->aArPermission->save($con);
				}
				$this->setArPermission($this->aArPermission);
			}

			if ($this->aArUserHasAccess !== null) {
				if ($this->aArUserHasAccess->isModified()) {
					$affectedRows += $this->aArUserHasAccess->save($con);
				}
				$this->setArUserHasAccess($this->aArUserHasAccess);
			}


						if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArPermissionToUserHasAccessPeer::doInsert($this, $con);
					$affectedRows += 1; 										 										 
					$this->setNew(false);
				} else {
					$affectedRows += ArPermissionToUserHasAccessPeer::doUpdate($this, $con);
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


												
			if ($this->aArPermission !== null) {
				if (!$this->aArPermission->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArPermission->getValidationFailures());
				}
			}

			if ($this->aArUserHasAccess !== null) {
				if (!$this->aArUserHasAccess->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArUserHasAccess->getValidationFailures());
				}
			}


			if (($retval = ArPermissionToUserHasAccessPeer::doValidate($this, $columns)) !== true) {
				$failureMap = array_merge($failureMap, $retval);
			}



			$this->alreadyInValidation = false;
		}

		return (!empty($failureMap) ? $failureMap : true);
	}

	
	public function getByName($name, $type = BasePeer::TYPE_PHPNAME)
	{
		$pos = ArPermissionToUserHasAccessPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
		return $this->getByPosition($pos);
	}

	
	public function getByPosition($pos)
	{
		switch($pos) {
			case 0:
				return $this->getArPermissionId();
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
		$keys = ArPermissionToUserHasAccessPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getArPermissionId(),
			$keys[1] => $this->getArUserHasAccessId(),
		);
		return $result;
	}

	
	public function setByName($name, $value, $type = BasePeer::TYPE_PHPNAME)
	{
		$pos = ArPermissionToUserHasAccessPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
		return $this->setByPosition($pos, $value);
	}

	
	public function setByPosition($pos, $value)
	{
		switch($pos) {
			case 0:
				$this->setArPermissionId($value);
				break;
			case 1:
				$this->setArUserHasAccessId($value);
				break;
		} 	}

	
	public function fromArray($arr, $keyType = BasePeer::TYPE_PHPNAME)
	{
		$keys = ArPermissionToUserHasAccessPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setArPermissionId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setArUserHasAccessId($arr[$keys[1]]);
	}

	
	public function buildCriteria()
	{
		$criteria = new Criteria(ArPermissionToUserHasAccessPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArPermissionToUserHasAccessPeer::AR_PERMISSION_ID)) $criteria->add(ArPermissionToUserHasAccessPeer::AR_PERMISSION_ID, $this->ar_permission_id);
		if ($this->isColumnModified(ArPermissionToUserHasAccessPeer::AR_USER_HAS_ACCESS_ID)) $criteria->add(ArPermissionToUserHasAccessPeer::AR_USER_HAS_ACCESS_ID, $this->ar_user_has_access_id);

		return $criteria;
	}

	
	public function buildPkeyCriteria()
	{
		$criteria = new Criteria(ArPermissionToUserHasAccessPeer::DATABASE_NAME);

		$criteria->add(ArPermissionToUserHasAccessPeer::AR_PERMISSION_ID, $this->ar_permission_id);
		$criteria->add(ArPermissionToUserHasAccessPeer::AR_USER_HAS_ACCESS_ID, $this->ar_user_has_access_id);

		return $criteria;
	}

	
	public function getPrimaryKey()
	{
		$pks = array();

		$pks[0] = $this->getArPermissionId();

		$pks[1] = $this->getArUserHasAccessId();

		return $pks;
	}

	
	public function setPrimaryKey($keys)
	{

		$this->setArPermissionId($keys[0]);

		$this->setArUserHasAccessId($keys[1]);

	}

	
	public function copyInto($copyObj, $deepCopy = false)
	{


		$copyObj->setNew(true);

		$copyObj->setArPermissionId(NULL); 
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
			self::$peer = new ArPermissionToUserHasAccessPeer();
		}
		return self::$peer;
	}

	
	public function setArPermission($v)
	{


		if ($v === null) {
			$this->setArPermissionId(NULL);
		} else {
			$this->setArPermissionId($v->getId());
		}


		$this->aArPermission = $v;
	}


	
	public function getArPermission($con = null)
	{
		if ($this->aArPermission === null && ($this->ar_permission_id !== null)) {
						include_once 'lib/model/om/BaseArPermissionPeer.php';

			$this->aArPermission = ArPermissionPeer::retrieveByPK($this->ar_permission_id, $con);

			
		}
		return $this->aArPermission;
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