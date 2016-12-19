<?php

/**
 * Base class that represents a row from the 'ar_communication_channel_type' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArCommunicationChannelType extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArCommunicationChannelTypePeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the name field.
	 * @var        string
	 */
	protected $name;

	/**
	 * The value for the description field.
	 * @var        string
	 */
	protected $description;

	/**
	 * The value for the internal_name field.
	 * @var        string
	 */
	protected $internal_name;

	/**
	 * @var        array ArCdr[] Collection to store aggregation of ArCdr objects.
	 */
	protected $collArCdrs;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArCdrs.
	 */
	private $lastArCdrCriteria = null;

	/**
	 * @var        array ArVendorDomain[] Collection to store aggregation of ArVendorDomain objects.
	 */
	protected $collArVendorDomains;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArVendorDomains.
	 */
	private $lastArVendorDomainCriteria = null;

	/**
	 * Flag to prevent endless save loop, if this object is referenced
	 * by another object which falls in this transaction.
	 * @var        boolean
	 */
	protected $alreadyInSave = false;

	/**
	 * Flag to prevent endless validation loop, if this object is referenced
	 * by another object which falls in this transaction.
	 * @var        boolean
	 */
	protected $alreadyInValidation = false;

	// symfony behavior
	
	const PEER = 'ArCommunicationChannelTypePeer';

	/**
	 * Get the [id] column value.
	 * 
	 * @return     int
	 */
	public function getId()
	{
		return $this->id;
	}

	/**
	 * Get the [name] column value.
	 * 
	 * @return     string
	 */
	public function getName()
	{
		return $this->name;
	}

	/**
	 * Get the [description] column value.
	 * 
	 * @return     string
	 */
	public function getDescription()
	{
		return $this->description;
	}

	/**
	 * Get the [internal_name] column value.
	 * 
	 * @return     string
	 */
	public function getInternalName()
	{
		return $this->internal_name;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCommunicationChannelType The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArCommunicationChannelTypePeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCommunicationChannelType The current object (for fluent API support)
	 */
	public function setName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->name !== $v) {
			$this->name = $v;
			$this->modifiedColumns[] = ArCommunicationChannelTypePeer::NAME;
		}

		return $this;
	} // setName()

	/**
	 * Set the value of [description] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCommunicationChannelType The current object (for fluent API support)
	 */
	public function setDescription($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->description !== $v) {
			$this->description = $v;
			$this->modifiedColumns[] = ArCommunicationChannelTypePeer::DESCRIPTION;
		}

		return $this;
	} // setDescription()

	/**
	 * Set the value of [internal_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCommunicationChannelType The current object (for fluent API support)
	 */
	public function setInternalName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->internal_name !== $v) {
			$this->internal_name = $v;
			$this->modifiedColumns[] = ArCommunicationChannelTypePeer::INTERNAL_NAME;
		}

		return $this;
	} // setInternalName()

	/**
	 * Indicates whether the columns in this object are only set to default values.
	 *
	 * This method can be used in conjunction with isModified() to indicate whether an object is both
	 * modified _and_ has some values set which are non-default.
	 *
	 * @return     boolean Whether the columns in this object are only been set with default values.
	 */
	public function hasOnlyDefaultValues()
	{
		// otherwise, everything was equal, so return TRUE
		return true;
	} // hasOnlyDefaultValues()

	/**
	 * Hydrates (populates) the object variables with values from the database resultset.
	 *
	 * An offset (0-based "start column") is specified so that objects can be hydrated
	 * with a subset of the columns in the resultset rows.  This is needed, for example,
	 * for results of JOIN queries where the resultset row includes columns from two or
	 * more tables.
	 *
	 * @param      array $row The row returned by PDOStatement->fetch(PDO::FETCH_NUM)
	 * @param      int $startcol 0-based offset column which indicates which restultset column to start with.
	 * @param      boolean $rehydrate Whether this object is being re-hydrated from the database.
	 * @return     int next starting column
	 * @throws     PropelException  - Any caught Exception will be rewrapped as a PropelException.
	 */
	public function hydrate($row, $startcol = 0, $rehydrate = false)
	{
		try {

			$this->id = ($row[$startcol + 0] !== null) ? (int) $row[$startcol + 0] : null;
			$this->name = ($row[$startcol + 1] !== null) ? (string) $row[$startcol + 1] : null;
			$this->description = ($row[$startcol + 2] !== null) ? (string) $row[$startcol + 2] : null;
			$this->internal_name = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 4; // 4 = ArCommunicationChannelTypePeer::NUM_COLUMNS - ArCommunicationChannelTypePeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArCommunicationChannelType object", $e);
		}
	}

	/**
	 * Checks and repairs the internal consistency of the object.
	 *
	 * This method is executed after an already-instantiated object is re-hydrated
	 * from the database.  It exists to check any foreign keys to make sure that
	 * the objects related to the current object are correct based on foreign key.
	 *
	 * You can override this method in the stub class, but you should always invoke
	 * the base method from the overridden method (i.e. parent::ensureConsistency()),
	 * in case your model changes.
	 *
	 * @throws     PropelException
	 */
	public function ensureConsistency()
	{

	} // ensureConsistency

	/**
	 * Reloads this object from datastore based on primary key and (optionally) resets all associated objects.
	 *
	 * This will only work if the object has been saved and has a valid primary key set.
	 *
	 * @param      boolean $deep (optional) Whether to also de-associated any related objects.
	 * @param      PropelPDO $con (optional) The PropelPDO connection to use.
	 * @return     void
	 * @throws     PropelException - if this object is deleted, unsaved or doesn't have pk match in db
	 */
	public function reload($deep = false, PropelPDO $con = null)
	{
		if ($this->isDeleted()) {
			throw new PropelException("Cannot reload a deleted object.");
		}

		if ($this->isNew()) {
			throw new PropelException("Cannot reload an unsaved object.");
		}

		if ($con === null) {
			$con = Propel::getConnection(ArCommunicationChannelTypePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArCommunicationChannelTypePeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

			$this->collArCdrs = null;
			$this->lastArCdrCriteria = null;

			$this->collArVendorDomains = null;
			$this->lastArVendorDomainCriteria = null;

		} // if (deep)
	}

	/**
	 * Removes this object from datastore and sets delete attribute.
	 *
	 * @param      PropelPDO $con
	 * @return     void
	 * @throws     PropelException
	 * @see        BaseObject::setDeleted()
	 * @see        BaseObject::isDeleted()
	 */
	public function delete(PropelPDO $con = null)
	{
		if ($this->isDeleted()) {
			throw new PropelException("This object has already been deleted.");
		}

		if ($con === null) {
			$con = Propel::getConnection(ArCommunicationChannelTypePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArCommunicationChannelTypePeer::doDelete($this, $con);
				$this->postDelete($con);
				$this->setDeleted(true);
				$con->commit();
			} else {
				$con->commit();
			}
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Persists this object to the database.
	 *
	 * If the object is new, it inserts it; otherwise an update is performed.
	 * All modified related objects will also be persisted in the doSave()
	 * method.  This method wraps all precipitate database operations in a
	 * single transaction.
	 *
	 * @param      PropelPDO $con
	 * @return     int The number of rows affected by this insert/update and any referring fk objects' save() operations.
	 * @throws     PropelException
	 * @see        doSave()
	 */
	public function save(PropelPDO $con = null)
	{
		if ($this->isDeleted()) {
			throw new PropelException("You cannot save an object that has been deleted.");
		}

		if ($con === null) {
			$con = Propel::getConnection(ArCommunicationChannelTypePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		$isInsert = $this->isNew();
		try {
			$ret = $this->preSave($con);
			if ($isInsert) {
				$ret = $ret && $this->preInsert($con);
			} else {
				$ret = $ret && $this->preUpdate($con);
			}
			if ($ret) {
				$affectedRows = $this->doSave($con);
				if ($isInsert) {
					$this->postInsert($con);
				} else {
					$this->postUpdate($con);
				}
				$this->postSave($con);
				ArCommunicationChannelTypePeer::addInstanceToPool($this);
			} else {
				$affectedRows = 0;
			}
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Performs the work of inserting or updating the row in the database.
	 *
	 * If the object is new, it inserts it; otherwise an update is performed.
	 * All related objects are also updated in this method.
	 *
	 * @param      PropelPDO $con
	 * @return     int The number of rows affected by this insert/update and any referring fk objects' save() operations.
	 * @throws     PropelException
	 * @see        save()
	 */
	protected function doSave(PropelPDO $con)
	{
		$affectedRows = 0; // initialize var to track total num of affected rows
		if (!$this->alreadyInSave) {
			$this->alreadyInSave = true;

			if ($this->isNew() ) {
				$this->modifiedColumns[] = ArCommunicationChannelTypePeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArCommunicationChannelTypePeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArCommunicationChannelTypePeer::doUpdate($this, $con);
				}

				$this->resetModified(); // [HL] After being saved an object is no longer 'modified'
			}

			if ($this->collArCdrs !== null) {
				foreach ($this->collArCdrs as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArVendorDomains !== null) {
				foreach ($this->collArVendorDomains as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			$this->alreadyInSave = false;

		}
		return $affectedRows;
	} // doSave()

	/**
	 * Array of ValidationFailed objects.
	 * @var        array ValidationFailed[]
	 */
	protected $validationFailures = array();

	/**
	 * Gets any ValidationFailed objects that resulted from last call to validate().
	 *
	 *
	 * @return     array ValidationFailed[]
	 * @see        validate()
	 */
	public function getValidationFailures()
	{
		return $this->validationFailures;
	}

	/**
	 * Validates the objects modified field values and all objects related to this table.
	 *
	 * If $columns is either a column name or an array of column names
	 * only those columns are validated.
	 *
	 * @param      mixed $columns Column name or an array of column names.
	 * @return     boolean Whether all columns pass validation.
	 * @see        doValidate()
	 * @see        getValidationFailures()
	 */
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

	/**
	 * This function performs the validation work for complex object models.
	 *
	 * In addition to checking the current object, all related objects will
	 * also be validated.  If all pass then <code>true</code> is returned; otherwise
	 * an aggreagated array of ValidationFailed objects will be returned.
	 *
	 * @param      array $columns Array of column names to validate.
	 * @return     mixed <code>true</code> if all validations pass; array of <code>ValidationFailed</code> objets otherwise.
	 */
	protected function doValidate($columns = null)
	{
		if (!$this->alreadyInValidation) {
			$this->alreadyInValidation = true;
			$retval = null;

			$failureMap = array();


			if (($retval = ArCommunicationChannelTypePeer::doValidate($this, $columns)) !== true) {
				$failureMap = array_merge($failureMap, $retval);
			}


				if ($this->collArCdrs !== null) {
					foreach ($this->collArCdrs as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArVendorDomains !== null) {
					foreach ($this->collArVendorDomains as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}


			$this->alreadyInValidation = false;
		}

		return (!empty($failureMap) ? $failureMap : true);
	}

	/**
	 * Retrieves a field from the object by name passed in as a string.
	 *
	 * @param      string $name name
	 * @param      string $type The type of fieldname the $name is of:
	 *                     one of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                     BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM
	 * @return     mixed Value of field.
	 */
	public function getByName($name, $type = BasePeer::TYPE_PHPNAME)
	{
		$pos = ArCommunicationChannelTypePeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
		$field = $this->getByPosition($pos);
		return $field;
	}

	/**
	 * Retrieves a field from the object by Position as specified in the xml schema.
	 * Zero-based.
	 *
	 * @param      int $pos position in xml schema
	 * @return     mixed Value of field at $pos
	 */
	public function getByPosition($pos)
	{
		switch($pos) {
			case 0:
				return $this->getId();
				break;
			case 1:
				return $this->getName();
				break;
			case 2:
				return $this->getDescription();
				break;
			case 3:
				return $this->getInternalName();
				break;
			default:
				return null;
				break;
		} // switch()
	}

	/**
	 * Exports the object as an array.
	 *
	 * You can specify the key type of the array by passing one of the class
	 * type constants.
	 *
	 * @param      string $keyType (optional) One of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                        BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM. Defaults to BasePeer::TYPE_PHPNAME.
	 * @param      boolean $includeLazyLoadColumns (optional) Whether to include lazy loaded columns.  Defaults to TRUE.
	 * @return     an associative array containing the field names (as keys) and field values
	 */
	public function toArray($keyType = BasePeer::TYPE_PHPNAME, $includeLazyLoadColumns = true)
	{
		$keys = ArCommunicationChannelTypePeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getName(),
			$keys[2] => $this->getDescription(),
			$keys[3] => $this->getInternalName(),
		);
		return $result;
	}

	/**
	 * Sets a field from the object by name passed in as a string.
	 *
	 * @param      string $name peer name
	 * @param      mixed $value field value
	 * @param      string $type The type of fieldname the $name is of:
	 *                     one of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                     BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM
	 * @return     void
	 */
	public function setByName($name, $value, $type = BasePeer::TYPE_PHPNAME)
	{
		$pos = ArCommunicationChannelTypePeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
		return $this->setByPosition($pos, $value);
	}

	/**
	 * Sets a field from the object by Position as specified in the xml schema.
	 * Zero-based.
	 *
	 * @param      int $pos position in xml schema
	 * @param      mixed $value field value
	 * @return     void
	 */
	public function setByPosition($pos, $value)
	{
		switch($pos) {
			case 0:
				$this->setId($value);
				break;
			case 1:
				$this->setName($value);
				break;
			case 2:
				$this->setDescription($value);
				break;
			case 3:
				$this->setInternalName($value);
				break;
		} // switch()
	}

	/**
	 * Populates the object using an array.
	 *
	 * This is particularly useful when populating an object from one of the
	 * request arrays (e.g. $_POST).  This method goes through the column
	 * names, checking to see whether a matching key exists in populated
	 * array. If so the setByName() method is called for that column.
	 *
	 * You can specify the key type of the array by additionally passing one
	 * of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME,
	 * BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM.
	 * The default key type is the column's phpname (e.g. 'AuthorId')
	 *
	 * @param      array  $arr     An array to populate the object from.
	 * @param      string $keyType The type of keys the array uses.
	 * @return     void
	 */
	public function fromArray($arr, $keyType = BasePeer::TYPE_PHPNAME)
	{
		$keys = ArCommunicationChannelTypePeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setName($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setDescription($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setInternalName($arr[$keys[3]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArCommunicationChannelTypePeer::DATABASE_NAME);

		if ($this->isColumnModified(ArCommunicationChannelTypePeer::ID)) $criteria->add(ArCommunicationChannelTypePeer::ID, $this->id);
		if ($this->isColumnModified(ArCommunicationChannelTypePeer::NAME)) $criteria->add(ArCommunicationChannelTypePeer::NAME, $this->name);
		if ($this->isColumnModified(ArCommunicationChannelTypePeer::DESCRIPTION)) $criteria->add(ArCommunicationChannelTypePeer::DESCRIPTION, $this->description);
		if ($this->isColumnModified(ArCommunicationChannelTypePeer::INTERNAL_NAME)) $criteria->add(ArCommunicationChannelTypePeer::INTERNAL_NAME, $this->internal_name);

		return $criteria;
	}

	/**
	 * Builds a Criteria object containing the primary key for this object.
	 *
	 * Unlike buildCriteria() this method includes the primary key values regardless
	 * of whether or not they have been modified.
	 *
	 * @return     Criteria The Criteria object containing value(s) for primary key(s).
	 */
	public function buildPkeyCriteria()
	{
		$criteria = new Criteria(ArCommunicationChannelTypePeer::DATABASE_NAME);

		$criteria->add(ArCommunicationChannelTypePeer::ID, $this->id);

		return $criteria;
	}

	/**
	 * Returns the primary key for this object (row).
	 * @return     int
	 */
	public function getPrimaryKey()
	{
		return $this->getId();
	}

	/**
	 * Generic method to set the primary key (id column).
	 *
	 * @param      int $key Primary key.
	 * @return     void
	 */
	public function setPrimaryKey($key)
	{
		$this->setId($key);
	}

	/**
	 * Sets contents of passed object to values from current object.
	 *
	 * If desired, this method can also make copies of all associated (fkey referrers)
	 * objects.
	 *
	 * @param      object $copyObj An object of ArCommunicationChannelType (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setName($this->name);

		$copyObj->setDescription($this->description);

		$copyObj->setInternalName($this->internal_name);


		if ($deepCopy) {
			// important: temporarily setNew(false) because this affects the behavior of
			// the getter/setter methods for fkey referrer objects.
			$copyObj->setNew(false);

			foreach ($this->getArCdrs() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArCdr($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArVendorDomains() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArVendorDomain($relObj->copy($deepCopy));
				}
			}

		} // if ($deepCopy)


		$copyObj->setNew(true);

		$copyObj->setId(NULL); // this is a auto-increment column, so set to default value

	}

	/**
	 * Makes a copy of this object that will be inserted as a new row in table when saved.
	 * It creates a new object filling in the simple attributes, but skipping any primary
	 * keys that are defined for the table.
	 *
	 * If desired, this method can also make copies of all associated (fkey referrers)
	 * objects.
	 *
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @return     ArCommunicationChannelType Clone of current object.
	 * @throws     PropelException
	 */
	public function copy($deepCopy = false)
	{
		// we use get_class(), because this might be a subclass
		$clazz = get_class($this);
		$copyObj = new $clazz();
		$this->copyInto($copyObj, $deepCopy);
		return $copyObj;
	}

	/**
	 * Returns a peer instance associated with this om.
	 *
	 * Since Peer classes are not to have any instance attributes, this method returns the
	 * same instance for all member of this class. The method could therefore
	 * be static, but this would prevent one from overriding the behavior.
	 *
	 * @return     ArCommunicationChannelTypePeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArCommunicationChannelTypePeer();
		}
		return self::$peer;
	}

	/**
	 * Clears out the collArCdrs collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArCdrs()
	 */
	public function clearArCdrs()
	{
		$this->collArCdrs = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArCdrs collection (array).
	 *
	 * By default this just sets the collArCdrs collection to an empty array (like clearcollArCdrs());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArCdrs()
	{
		$this->collArCdrs = array();
	}

	/**
	 * Gets an array of ArCdr objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArCommunicationChannelType has previously been saved, it will retrieve
	 * related ArCdrs from storage. If this ArCommunicationChannelType is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArCdr[]
	 * @throws     PropelException
	 */
	public function getArCdrs($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArCommunicationChannelTypePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArCdrs === null) {
			if ($this->isNew()) {
			   $this->collArCdrs = array();
			} else {

				$criteria->add(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

				ArCdrPeer::addSelectColumns($criteria);
				$this->collArCdrs = ArCdrPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

				ArCdrPeer::addSelectColumns($criteria);
				if (!isset($this->lastArCdrCriteria) || !$this->lastArCdrCriteria->equals($criteria)) {
					$this->collArCdrs = ArCdrPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArCdrCriteria = $criteria;
		return $this->collArCdrs;
	}

	/**
	 * Returns the number of related ArCdr objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArCdr objects.
	 * @throws     PropelException
	 */
	public function countArCdrs(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArCommunicationChannelTypePeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArCdrs === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

				$count = ArCdrPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

				if (!isset($this->lastArCdrCriteria) || !$this->lastArCdrCriteria->equals($criteria)) {
					$count = ArCdrPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArCdrs);
				}
			} else {
				$count = count($this->collArCdrs);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArCdr object to this object
	 * through the ArCdr foreign key attribute.
	 *
	 * @param      ArCdr $l ArCdr
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArCdr(ArCdr $l)
	{
		if ($this->collArCdrs === null) {
			$this->initArCdrs();
		}
		if (!in_array($l, $this->collArCdrs, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArCdrs, $l);
			$l->setArCommunicationChannelType($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArCommunicationChannelType is new, it will return
	 * an empty collection; or if this ArCommunicationChannelType has previously
	 * been saved, it will retrieve related ArCdrs from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArCommunicationChannelType.
	 */
	public function getArCdrsJoinArOrganizationUnit($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArCommunicationChannelTypePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArCdrs === null) {
			if ($this->isNew()) {
				$this->collArCdrs = array();
			} else {

				$criteria->add(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

				$this->collArCdrs = ArCdrPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

			if (!isset($this->lastArCdrCriteria) || !$this->lastArCdrCriteria->equals($criteria)) {
				$this->collArCdrs = ArCdrPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		}
		$this->lastArCdrCriteria = $criteria;

		return $this->collArCdrs;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArCommunicationChannelType is new, it will return
	 * an empty collection; or if this ArCommunicationChannelType has previously
	 * been saved, it will retrieve related ArCdrs from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArCommunicationChannelType.
	 */
	public function getArCdrsJoinArVendor($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArCommunicationChannelTypePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArCdrs === null) {
			if ($this->isNew()) {
				$this->collArCdrs = array();
			} else {

				$criteria->add(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

				$this->collArCdrs = ArCdrPeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

			if (!isset($this->lastArCdrCriteria) || !$this->lastArCdrCriteria->equals($criteria)) {
				$this->collArCdrs = ArCdrPeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		}
		$this->lastArCdrCriteria = $criteria;

		return $this->collArCdrs;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArCommunicationChannelType is new, it will return
	 * an empty collection; or if this ArCommunicationChannelType has previously
	 * been saved, it will retrieve related ArCdrs from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArCommunicationChannelType.
	 */
	public function getArCdrsJoinArTelephonePrefix($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArCommunicationChannelTypePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArCdrs === null) {
			if ($this->isNew()) {
				$this->collArCdrs = array();
			} else {

				$criteria->add(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

				$this->collArCdrs = ArCdrPeer::doSelectJoinArTelephonePrefix($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

			if (!isset($this->lastArCdrCriteria) || !$this->lastArCdrCriteria->equals($criteria)) {
				$this->collArCdrs = ArCdrPeer::doSelectJoinArTelephonePrefix($criteria, $con, $join_behavior);
			}
		}
		$this->lastArCdrCriteria = $criteria;

		return $this->collArCdrs;
	}

	/**
	 * Clears out the collArVendorDomains collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArVendorDomains()
	 */
	public function clearArVendorDomains()
	{
		$this->collArVendorDomains = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArVendorDomains collection (array).
	 *
	 * By default this just sets the collArVendorDomains collection to an empty array (like clearcollArVendorDomains());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArVendorDomains()
	{
		$this->collArVendorDomains = array();
	}

	/**
	 * Gets an array of ArVendorDomain objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArCommunicationChannelType has previously been saved, it will retrieve
	 * related ArVendorDomains from storage. If this ArCommunicationChannelType is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArVendorDomain[]
	 * @throws     PropelException
	 */
	public function getArVendorDomains($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArCommunicationChannelTypePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArVendorDomains === null) {
			if ($this->isNew()) {
			   $this->collArVendorDomains = array();
			} else {

				$criteria->add(ArVendorDomainPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

				ArVendorDomainPeer::addSelectColumns($criteria);
				$this->collArVendorDomains = ArVendorDomainPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArVendorDomainPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

				ArVendorDomainPeer::addSelectColumns($criteria);
				if (!isset($this->lastArVendorDomainCriteria) || !$this->lastArVendorDomainCriteria->equals($criteria)) {
					$this->collArVendorDomains = ArVendorDomainPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArVendorDomainCriteria = $criteria;
		return $this->collArVendorDomains;
	}

	/**
	 * Returns the number of related ArVendorDomain objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArVendorDomain objects.
	 * @throws     PropelException
	 */
	public function countArVendorDomains(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArCommunicationChannelTypePeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArVendorDomains === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArVendorDomainPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

				$count = ArVendorDomainPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArVendorDomainPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

				if (!isset($this->lastArVendorDomainCriteria) || !$this->lastArVendorDomainCriteria->equals($criteria)) {
					$count = ArVendorDomainPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArVendorDomains);
				}
			} else {
				$count = count($this->collArVendorDomains);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArVendorDomain object to this object
	 * through the ArVendorDomain foreign key attribute.
	 *
	 * @param      ArVendorDomain $l ArVendorDomain
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArVendorDomain(ArVendorDomain $l)
	{
		if ($this->collArVendorDomains === null) {
			$this->initArVendorDomains();
		}
		if (!in_array($l, $this->collArVendorDomains, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArVendorDomains, $l);
			$l->setArCommunicationChannelType($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArCommunicationChannelType is new, it will return
	 * an empty collection; or if this ArCommunicationChannelType has previously
	 * been saved, it will retrieve related ArVendorDomains from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArCommunicationChannelType.
	 */
	public function getArVendorDomainsJoinArVendor($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArCommunicationChannelTypePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArVendorDomains === null) {
			if ($this->isNew()) {
				$this->collArVendorDomains = array();
			} else {

				$criteria->add(ArVendorDomainPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

				$this->collArVendorDomains = ArVendorDomainPeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArVendorDomainPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->id);

			if (!isset($this->lastArVendorDomainCriteria) || !$this->lastArVendorDomainCriteria->equals($criteria)) {
				$this->collArVendorDomains = ArVendorDomainPeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		}
		$this->lastArVendorDomainCriteria = $criteria;

		return $this->collArVendorDomains;
	}

	/**
	 * Resets all collections of referencing foreign keys.
	 *
	 * This method is a user-space workaround for PHP's inability to garbage collect objects
	 * with circular references.  This is currently necessary when using Propel in certain
	 * daemon or large-volumne/high-memory operations.
	 *
	 * @param      boolean $deep Whether to also clear the references on all associated objects.
	 */
	public function clearAllReferences($deep = false)
	{
		if ($deep) {
			if ($this->collArCdrs) {
				foreach ((array) $this->collArCdrs as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArVendorDomains) {
				foreach ((array) $this->collArVendorDomains as $o) {
					$o->clearAllReferences($deep);
				}
			}
		} // if ($deep)

		$this->collArCdrs = null;
		$this->collArVendorDomains = null;
	}

} // BaseArCommunicationChannelType
