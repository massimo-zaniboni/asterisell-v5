<?php


abstract class BaseArPermissionToRole extends BaseObject  implements Persistent {


	
	protected static $peer;


	
	protected $ar_permission_id;


	
	protected $ar_role_id;

	
	protected $aArPermission;

	
	protected $aArRole;

	
	protected $alreadyInSave = false;

	
	protected $alreadyInValidation = false;

	
	public function getArPermissionId()
	{

		return $this->ar_permission_id;
	}

	
	public function getArRoleId()
	{

		return $this->ar_role_id;
	}

	
	public function setArPermissionId($v)
	{

						if ($v !== null && !is_int($v) && is_numeric($v)) {
			$v = (int) $v;
		}

		if ($this->ar_permission_id !== $v) {
			$this->ar_permission_id = $v;
			$this->modifiedColumns[] = ArPermissionToRolePeer::AR_PERMISSION_ID;
		}

		if ($this->aArPermission !== null && $this->aArPermission->getId() !== $v) {
			$this->aArPermission = null;
		}

	} 
	
	public function setArRoleId($v)
	{

						if ($v !== null && !is_int($v) && is_numeric($v)) {
			$v = (int) $v;
		}

		if ($this->ar_role_id !== $v) {
			$this->ar_role_id = $v;
			$this->modifiedColumns[] = ArPermissionToRolePeer::AR_ROLE_ID;
		}

		if ($this->aArRole !== null && $this->aArRole->getId() !== $v) {
			$this->aArRole = null;
		}

	} 
	
	public function hydrate(ResultSet $rs, $startcol = 1)
	{
		try {

			$this->ar_permission_id = $rs->getInt($startcol + 0);

			$this->ar_role_id = $rs->getInt($startcol + 1);

			$this->resetModified();

			$this->setNew(false);

						return $startcol + 2; 
		} catch (Exception $e) {
			throw new PropelException("Error populating ArPermissionToRole object", $e);
		}
	}

	
	public function delete($con = null)
	{
		if ($this->isDeleted()) {
			throw new PropelException("This object has already been deleted.");
		}

		if ($con === null) {
			$con = Propel::getConnection(ArPermissionToRolePeer::DATABASE_NAME);
		}

		try {
			$con->begin();
			ArPermissionToRolePeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArPermissionToRolePeer::DATABASE_NAME);
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

			if ($this->aArRole !== null) {
				if ($this->aArRole->isModified()) {
					$affectedRows += $this->aArRole->save($con);
				}
				$this->setArRole($this->aArRole);
			}


						if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArPermissionToRolePeer::doInsert($this, $con);
					$affectedRows += 1; 										 										 
					$this->setNew(false);
				} else {
					$affectedRows += ArPermissionToRolePeer::doUpdate($this, $con);
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

			if ($this->aArRole !== null) {
				if (!$this->aArRole->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArRole->getValidationFailures());
				}
			}


			if (($retval = ArPermissionToRolePeer::doValidate($this, $columns)) !== true) {
				$failureMap = array_merge($failureMap, $retval);
			}



			$this->alreadyInValidation = false;
		}

		return (!empty($failureMap) ? $failureMap : true);
	}

	
	public function getByName($name, $type = BasePeer::TYPE_PHPNAME)
	{
		$pos = ArPermissionToRolePeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
		return $this->getByPosition($pos);
	}

	
	public function getByPosition($pos)
	{
		switch($pos) {
			case 0:
				return $this->getArPermissionId();
				break;
			case 1:
				return $this->getArRoleId();
				break;
			default:
				return null;
				break;
		} 	}

	
	public function toArray($keyType = BasePeer::TYPE_PHPNAME)
	{
		$keys = ArPermissionToRolePeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getArPermissionId(),
			$keys[1] => $this->getArRoleId(),
		);
		return $result;
	}

	
	public function setByName($name, $value, $type = BasePeer::TYPE_PHPNAME)
	{
		$pos = ArPermissionToRolePeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
		return $this->setByPosition($pos, $value);
	}

	
	public function setByPosition($pos, $value)
	{
		switch($pos) {
			case 0:
				$this->setArPermissionId($value);
				break;
			case 1:
				$this->setArRoleId($value);
				break;
		} 	}

	
	public function fromArray($arr, $keyType = BasePeer::TYPE_PHPNAME)
	{
		$keys = ArPermissionToRolePeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setArPermissionId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setArRoleId($arr[$keys[1]]);
	}

	
	public function buildCriteria()
	{
		$criteria = new Criteria(ArPermissionToRolePeer::DATABASE_NAME);

		if ($this->isColumnModified(ArPermissionToRolePeer::AR_PERMISSION_ID)) $criteria->add(ArPermissionToRolePeer::AR_PERMISSION_ID, $this->ar_permission_id);
		if ($this->isColumnModified(ArPermissionToRolePeer::AR_ROLE_ID)) $criteria->add(ArPermissionToRolePeer::AR_ROLE_ID, $this->ar_role_id);

		return $criteria;
	}

	
	public function buildPkeyCriteria()
	{
		$criteria = new Criteria(ArPermissionToRolePeer::DATABASE_NAME);

		$criteria->add(ArPermissionToRolePeer::AR_PERMISSION_ID, $this->ar_permission_id);
		$criteria->add(ArPermissionToRolePeer::AR_ROLE_ID, $this->ar_role_id);

		return $criteria;
	}

	
	public function getPrimaryKey()
	{
		$pks = array();

		$pks[0] = $this->getArPermissionId();

		$pks[1] = $this->getArRoleId();

		return $pks;
	}

	
	public function setPrimaryKey($keys)
	{

		$this->setArPermissionId($keys[0]);

		$this->setArRoleId($keys[1]);

	}

	
	public function copyInto($copyObj, $deepCopy = false)
	{


		$copyObj->setNew(true);

		$copyObj->setArPermissionId(NULL); 
		$copyObj->setArRoleId(NULL); 
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
			self::$peer = new ArPermissionToRolePeer();
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

} 