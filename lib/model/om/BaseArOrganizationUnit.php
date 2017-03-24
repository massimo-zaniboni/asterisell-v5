<?php

/**
 * Base class that represents a row from the 'ar_organization_unit' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArOrganizationUnit extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArOrganizationUnitPeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the internal_name field.
	 * @var        string
	 */
	protected $internal_name;

	/**
	 * The value for the internal_name2 field.
	 * @var        string
	 */
	protected $internal_name2;

	/**
	 * The value for the export_code field.
	 * @var        string
	 */
	protected $export_code;

	/**
	 * The value for the automatically_managed_from field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $automatically_managed_from;

	/**
	 * @var        array ArCdr[] Collection to store aggregation of ArCdr objects.
	 */
	protected $collArCdrs;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArCdrs.
	 */
	private $lastArCdrCriteria = null;

	/**
	 * @var        array ArOrganizationUnitHasStructure[] Collection to store aggregation of ArOrganizationUnitHasStructure objects.
	 */
	protected $collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId.
	 */
	private $lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria = null;

	/**
	 * @var        array ArOrganizationUnitHasStructure[] Collection to store aggregation of ArOrganizationUnitHasStructure objects.
	 */
	protected $collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId.
	 */
	private $lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria = null;

	/**
	 * @var        array ArExpandedExtensions[] Collection to store aggregation of ArExpandedExtensions objects.
	 */
	protected $collArExpandedExtensionss;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArExpandedExtensionss.
	 */
	private $lastArExpandedExtensionsCriteria = null;

	/**
	 * @var        array ArUser[] Collection to store aggregation of ArUser objects.
	 */
	protected $collArUsers;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArUsers.
	 */
	private $lastArUserCriteria = null;

	/**
	 * @var        array ArReport[] Collection to store aggregation of ArReport objects.
	 */
	protected $collArReports;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArReports.
	 */
	private $lastArReportCriteria = null;

	/**
	 * @var        array ArReportScheduler[] Collection to store aggregation of ArReportScheduler objects.
	 */
	protected $collArReportSchedulers;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArReportSchedulers.
	 */
	private $lastArReportSchedulerCriteria = null;

	/**
	 * @var        array ArPostponedReport[] Collection to store aggregation of ArPostponedReport objects.
	 */
	protected $collArPostponedReports;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArPostponedReports.
	 */
	private $lastArPostponedReportCriteria = null;

	/**
	 * @var        ArPostponedReportTmp one-to-one related ArPostponedReportTmp object
	 */
	protected $singleArPostponedReportTmp;

	/**
	 * @var        array ArReportToReadUserView[] Collection to store aggregation of ArReportToReadUserView objects.
	 */
	protected $collArReportToReadUserViews;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArReportToReadUserViews.
	 */
	private $lastArReportToReadUserViewCriteria = null;

	/**
	 * @var        array ArInstanceStatus[] Collection to store aggregation of ArInstanceStatus objects.
	 */
	protected $collArInstanceStatuss;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArInstanceStatuss.
	 */
	private $lastArInstanceStatusCriteria = null;

	/**
	 * @var        array ArAssignedService[] Collection to store aggregation of ArAssignedService objects.
	 */
	protected $collArAssignedServices;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArAssignedServices.
	 */
	private $lastArAssignedServiceCriteria = null;

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
	
	const PEER = 'ArOrganizationUnitPeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->automatically_managed_from = 0;
	}

	/**
	 * Initializes internal state of BaseArOrganizationUnit object.
	 * @see        applyDefaults()
	 */
	public function __construct()
	{
		parent::__construct();
		$this->applyDefaultValues();
	}

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
	 * Get the [internal_name] column value.
	 * 
	 * @return     string
	 */
	public function getInternalName()
	{
		return $this->internal_name;
	}

	/**
	 * Get the [internal_name2] column value.
	 * 
	 * @return     string
	 */
	public function getInternalName2()
	{
		return $this->internal_name2;
	}

	/**
	 * Get the [export_code] column value.
	 * 
	 * @return     string
	 */
	public function getExportCode()
	{
		return $this->export_code;
	}

	/**
	 * Get the [automatically_managed_from] column value.
	 * 
	 * @return     int
	 */
	public function getAutomaticallyManagedFrom()
	{
		return $this->automatically_managed_from;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArOrganizationUnit The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArOrganizationUnitPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [internal_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArOrganizationUnit The current object (for fluent API support)
	 */
	public function setInternalName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->internal_name !== $v) {
			$this->internal_name = $v;
			$this->modifiedColumns[] = ArOrganizationUnitPeer::INTERNAL_NAME;
		}

		return $this;
	} // setInternalName()

	/**
	 * Set the value of [internal_name2] column.
	 * 
	 * @param      string $v new value
	 * @return     ArOrganizationUnit The current object (for fluent API support)
	 */
	public function setInternalName2($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->internal_name2 !== $v) {
			$this->internal_name2 = $v;
			$this->modifiedColumns[] = ArOrganizationUnitPeer::INTERNAL_NAME2;
		}

		return $this;
	} // setInternalName2()

	/**
	 * Set the value of [export_code] column.
	 * 
	 * @param      string $v new value
	 * @return     ArOrganizationUnit The current object (for fluent API support)
	 */
	public function setExportCode($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->export_code !== $v) {
			$this->export_code = $v;
			$this->modifiedColumns[] = ArOrganizationUnitPeer::EXPORT_CODE;
		}

		return $this;
	} // setExportCode()

	/**
	 * Set the value of [automatically_managed_from] column.
	 * 
	 * @param      int $v new value
	 * @return     ArOrganizationUnit The current object (for fluent API support)
	 */
	public function setAutomaticallyManagedFrom($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->automatically_managed_from !== $v || $this->isNew()) {
			$this->automatically_managed_from = $v;
			$this->modifiedColumns[] = ArOrganizationUnitPeer::AUTOMATICALLY_MANAGED_FROM;
		}

		return $this;
	} // setAutomaticallyManagedFrom()

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
			if ($this->automatically_managed_from !== 0) {
				return false;
			}

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
			$this->internal_name = ($row[$startcol + 1] !== null) ? (string) $row[$startcol + 1] : null;
			$this->internal_name2 = ($row[$startcol + 2] !== null) ? (string) $row[$startcol + 2] : null;
			$this->export_code = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->automatically_managed_from = ($row[$startcol + 4] !== null) ? (int) $row[$startcol + 4] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 5; // 5 = ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArOrganizationUnit object", $e);
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
			$con = Propel::getConnection(ArOrganizationUnitPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArOrganizationUnitPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

			$this->collArCdrs = null;
			$this->lastArCdrCriteria = null;

			$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = null;
			$this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria = null;

			$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = null;
			$this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria = null;

			$this->collArExpandedExtensionss = null;
			$this->lastArExpandedExtensionsCriteria = null;

			$this->collArUsers = null;
			$this->lastArUserCriteria = null;

			$this->collArReports = null;
			$this->lastArReportCriteria = null;

			$this->collArReportSchedulers = null;
			$this->lastArReportSchedulerCriteria = null;

			$this->collArPostponedReports = null;
			$this->lastArPostponedReportCriteria = null;

			$this->singleArPostponedReportTmp = null;

			$this->collArReportToReadUserViews = null;
			$this->lastArReportToReadUserViewCriteria = null;

			$this->collArInstanceStatuss = null;
			$this->lastArInstanceStatusCriteria = null;

			$this->collArAssignedServices = null;
			$this->lastArAssignedServiceCriteria = null;

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
			$con = Propel::getConnection(ArOrganizationUnitPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArOrganizationUnitPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArOrganizationUnitPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArOrganizationUnitPeer::addInstanceToPool($this);
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
				$this->modifiedColumns[] = ArOrganizationUnitPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArOrganizationUnitPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArOrganizationUnitPeer::doUpdate($this, $con);
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

			if ($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId !== null) {
				foreach ($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId !== null) {
				foreach ($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArExpandedExtensionss !== null) {
				foreach ($this->collArExpandedExtensionss as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArUsers !== null) {
				foreach ($this->collArUsers as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArReports !== null) {
				foreach ($this->collArReports as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArReportSchedulers !== null) {
				foreach ($this->collArReportSchedulers as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArPostponedReports !== null) {
				foreach ($this->collArPostponedReports as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->singleArPostponedReportTmp !== null) {
				if (!$this->singleArPostponedReportTmp->isDeleted()) {
						$affectedRows += $this->singleArPostponedReportTmp->save($con);
				}
			}

			if ($this->collArReportToReadUserViews !== null) {
				foreach ($this->collArReportToReadUserViews as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArInstanceStatuss !== null) {
				foreach ($this->collArInstanceStatuss as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArAssignedServices !== null) {
				foreach ($this->collArAssignedServices as $referrerFK) {
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


			if (($retval = ArOrganizationUnitPeer::doValidate($this, $columns)) !== true) {
				$failureMap = array_merge($failureMap, $retval);
			}


				if ($this->collArCdrs !== null) {
					foreach ($this->collArCdrs as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId !== null) {
					foreach ($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId !== null) {
					foreach ($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArExpandedExtensionss !== null) {
					foreach ($this->collArExpandedExtensionss as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArUsers !== null) {
					foreach ($this->collArUsers as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArReports !== null) {
					foreach ($this->collArReports as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArReportSchedulers !== null) {
					foreach ($this->collArReportSchedulers as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArPostponedReports !== null) {
					foreach ($this->collArPostponedReports as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->singleArPostponedReportTmp !== null) {
					if (!$this->singleArPostponedReportTmp->validate($columns)) {
						$failureMap = array_merge($failureMap, $this->singleArPostponedReportTmp->getValidationFailures());
					}
				}

				if ($this->collArReportToReadUserViews !== null) {
					foreach ($this->collArReportToReadUserViews as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArInstanceStatuss !== null) {
					foreach ($this->collArInstanceStatuss as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArAssignedServices !== null) {
					foreach ($this->collArAssignedServices as $referrerFK) {
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
		$pos = ArOrganizationUnitPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getInternalName();
				break;
			case 2:
				return $this->getInternalName2();
				break;
			case 3:
				return $this->getExportCode();
				break;
			case 4:
				return $this->getAutomaticallyManagedFrom();
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
		$keys = ArOrganizationUnitPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getInternalName(),
			$keys[2] => $this->getInternalName2(),
			$keys[3] => $this->getExportCode(),
			$keys[4] => $this->getAutomaticallyManagedFrom(),
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
		$pos = ArOrganizationUnitPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setInternalName($value);
				break;
			case 2:
				$this->setInternalName2($value);
				break;
			case 3:
				$this->setExportCode($value);
				break;
			case 4:
				$this->setAutomaticallyManagedFrom($value);
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
		$keys = ArOrganizationUnitPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setInternalName($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setInternalName2($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setExportCode($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setAutomaticallyManagedFrom($arr[$keys[4]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArOrganizationUnitPeer::ID)) $criteria->add(ArOrganizationUnitPeer::ID, $this->id);
		if ($this->isColumnModified(ArOrganizationUnitPeer::INTERNAL_NAME)) $criteria->add(ArOrganizationUnitPeer::INTERNAL_NAME, $this->internal_name);
		if ($this->isColumnModified(ArOrganizationUnitPeer::INTERNAL_NAME2)) $criteria->add(ArOrganizationUnitPeer::INTERNAL_NAME2, $this->internal_name2);
		if ($this->isColumnModified(ArOrganizationUnitPeer::EXPORT_CODE)) $criteria->add(ArOrganizationUnitPeer::EXPORT_CODE, $this->export_code);
		if ($this->isColumnModified(ArOrganizationUnitPeer::AUTOMATICALLY_MANAGED_FROM)) $criteria->add(ArOrganizationUnitPeer::AUTOMATICALLY_MANAGED_FROM, $this->automatically_managed_from);

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
		$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);

		$criteria->add(ArOrganizationUnitPeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArOrganizationUnit (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setInternalName($this->internal_name);

		$copyObj->setInternalName2($this->internal_name2);

		$copyObj->setExportCode($this->export_code);

		$copyObj->setAutomaticallyManagedFrom($this->automatically_managed_from);


		if ($deepCopy) {
			// important: temporarily setNew(false) because this affects the behavior of
			// the getter/setter methods for fkey referrer objects.
			$copyObj->setNew(false);

			foreach ($this->getArCdrs() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArCdr($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArOrganizationUnitHasStructureRelatedByArOrganizationUnitId($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitId($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArExpandedExtensionss() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArExpandedExtensions($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArUsers() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArUser($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArReports() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArReport($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArReportSchedulers() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArReportScheduler($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArPostponedReports() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArPostponedReport($relObj->copy($deepCopy));
				}
			}

			$relObj = $this->getArPostponedReportTmp();
			if ($relObj) {
				$copyObj->setArPostponedReportTmp($relObj->copy($deepCopy));
			}

			foreach ($this->getArReportToReadUserViews() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArReportToReadUserView($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArInstanceStatuss() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArInstanceStatus($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArAssignedServices() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArAssignedService($relObj->copy($deepCopy));
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
	 * @return     ArOrganizationUnit Clone of current object.
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
	 * @return     ArOrganizationUnitPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArOrganizationUnitPeer();
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
	 * Otherwise if this ArOrganizationUnit has previously been saved, it will retrieve
	 * related ArCdrs from storage. If this ArOrganizationUnit is new, it will return
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
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArCdrs === null) {
			if ($this->isNew()) {
			   $this->collArCdrs = array();
			} else {

				$criteria->add(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArCdrPeer::addSelectColumns($criteria);
				$this->collArCdrs = ArCdrPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

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
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
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

				$criteria->add(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$count = ArCdrPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

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
			$l->setArOrganizationUnit($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArCdrs from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArCdrsJoinArVendor($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArCdrs === null) {
			if ($this->isNew()) {
				$this->collArCdrs = array();
			} else {

				$criteria->add(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArCdrs = ArCdrPeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

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
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArCdrs from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArCdrsJoinArCommunicationChannelType($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArCdrs === null) {
			if ($this->isNew()) {
				$this->collArCdrs = array();
			} else {

				$criteria->add(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArCdrs = ArCdrPeer::doSelectJoinArCommunicationChannelType($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArCdrCriteria) || !$this->lastArCdrCriteria->equals($criteria)) {
				$this->collArCdrs = ArCdrPeer::doSelectJoinArCommunicationChannelType($criteria, $con, $join_behavior);
			}
		}
		$this->lastArCdrCriteria = $criteria;

		return $this->collArCdrs;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArCdrs from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArCdrsJoinArTelephonePrefix($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArCdrs === null) {
			if ($this->isNew()) {
				$this->collArCdrs = array();
			} else {

				$criteria->add(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArCdrs = ArCdrPeer::doSelectJoinArTelephonePrefix($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArCdrCriteria) || !$this->lastArCdrCriteria->equals($criteria)) {
				$this->collArCdrs = ArCdrPeer::doSelectJoinArTelephonePrefix($criteria, $con, $join_behavior);
			}
		}
		$this->lastArCdrCriteria = $criteria;

		return $this->collArCdrs;
	}

	/**
	 * Clears out the collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId()
	 */
	public function clearArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId()
	{
		$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId collection (array).
	 *
	 * By default this just sets the collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId collection to an empty array (like clearcollArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId()
	{
		$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = array();
	}

	/**
	 * Gets an array of ArOrganizationUnitHasStructure objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit has previously been saved, it will retrieve
	 * related ArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId from storage. If this ArOrganizationUnit is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArOrganizationUnitHasStructure[]
	 * @throws     PropelException
	 */
	public function getArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId === null) {
			if ($this->isNew()) {
			   $this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = array();
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
				$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
				if (!isset($this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria) || !$this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria->equals($criteria)) {
					$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria = $criteria;
		return $this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId;
	}

	/**
	 * Returns the number of related ArOrganizationUnitHasStructure objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArOrganizationUnitHasStructure objects.
	 * @throws     PropelException
	 */
	public function countArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$count = ArOrganizationUnitHasStructurePeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				if (!isset($this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria) || !$this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria->equals($criteria)) {
					$count = ArOrganizationUnitHasStructurePeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId);
				}
			} else {
				$count = count($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArOrganizationUnitHasStructure object to this object
	 * through the ArOrganizationUnitHasStructure foreign key attribute.
	 *
	 * @param      ArOrganizationUnitHasStructure $l ArOrganizationUnitHasStructure
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArOrganizationUnitHasStructureRelatedByArOrganizationUnitId(ArOrganizationUnitHasStructure $l)
	{
		if ($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId === null) {
			$this->initArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId();
		}
		if (!in_array($l, $this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId, $l);
			$l->setArOrganizationUnitRelatedByArOrganizationUnitId($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArOrganizationUnitHasStructuresRelatedByArOrganizationUnitIdJoinArOrganizationUnitType($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId === null) {
			if ($this->isNew()) {
				$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = array();
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelectJoinArOrganizationUnitType($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria) || !$this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria->equals($criteria)) {
				$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelectJoinArOrganizationUnitType($criteria, $con, $join_behavior);
			}
		}
		$this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria = $criteria;

		return $this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArOrganizationUnitHasStructuresRelatedByArOrganizationUnitIdJoinArRateCategory($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId === null) {
			if ($this->isNew()) {
				$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = array();
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelectJoinArRateCategory($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria) || !$this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria->equals($criteria)) {
				$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelectJoinArRateCategory($criteria, $con, $join_behavior);
			}
		}
		$this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria = $criteria;

		return $this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArOrganizationUnitHasStructuresRelatedByArOrganizationUnitIdJoinArParty($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId === null) {
			if ($this->isNew()) {
				$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = array();
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelectJoinArParty($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria) || !$this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria->equals($criteria)) {
				$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelectJoinArParty($criteria, $con, $join_behavior);
			}
		}
		$this->lastArOrganizationUnitHasStructureRelatedByArOrganizationUnitIdCriteria = $criteria;

		return $this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId;
	}

	/**
	 * Clears out the collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId()
	 */
	public function clearArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId()
	{
		$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId collection (array).
	 *
	 * By default this just sets the collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId collection to an empty array (like clearcollArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId()
	{
		$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = array();
	}

	/**
	 * Gets an array of ArOrganizationUnitHasStructure objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit has previously been saved, it will retrieve
	 * related ArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId from storage. If this ArOrganizationUnit is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArOrganizationUnitHasStructure[]
	 * @throws     PropelException
	 */
	public function getArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId === null) {
			if ($this->isNew()) {
			   $this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = array();
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, $this->id);

				ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
				$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, $this->id);

				ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
				if (!isset($this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria) || !$this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria->equals($criteria)) {
					$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria = $criteria;
		return $this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId;
	}

	/**
	 * Returns the number of related ArOrganizationUnitHasStructure objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArOrganizationUnitHasStructure objects.
	 * @throws     PropelException
	 */
	public function countArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, $this->id);

				$count = ArOrganizationUnitHasStructurePeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, $this->id);

				if (!isset($this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria) || !$this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria->equals($criteria)) {
					$count = ArOrganizationUnitHasStructurePeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId);
				}
			} else {
				$count = count($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArOrganizationUnitHasStructure object to this object
	 * through the ArOrganizationUnitHasStructure foreign key attribute.
	 *
	 * @param      ArOrganizationUnitHasStructure $l ArOrganizationUnitHasStructure
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitId(ArOrganizationUnitHasStructure $l)
	{
		if ($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId === null) {
			$this->initArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId();
		}
		if (!in_array($l, $this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId, $l);
			$l->setArOrganizationUnitRelatedByArParentOrganizationUnitId($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitIdJoinArOrganizationUnitType($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId === null) {
			if ($this->isNew()) {
				$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = array();
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelectJoinArOrganizationUnitType($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria) || !$this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria->equals($criteria)) {
				$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelectJoinArOrganizationUnitType($criteria, $con, $join_behavior);
			}
		}
		$this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria = $criteria;

		return $this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitIdJoinArRateCategory($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId === null) {
			if ($this->isNew()) {
				$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = array();
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelectJoinArRateCategory($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria) || !$this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria->equals($criteria)) {
				$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelectJoinArRateCategory($criteria, $con, $join_behavior);
			}
		}
		$this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria = $criteria;

		return $this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitIdJoinArParty($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId === null) {
			if ($this->isNew()) {
				$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = array();
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelectJoinArParty($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria) || !$this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria->equals($criteria)) {
				$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = ArOrganizationUnitHasStructurePeer::doSelectJoinArParty($criteria, $con, $join_behavior);
			}
		}
		$this->lastArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitIdCriteria = $criteria;

		return $this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId;
	}

	/**
	 * Clears out the collArExpandedExtensionss collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArExpandedExtensionss()
	 */
	public function clearArExpandedExtensionss()
	{
		$this->collArExpandedExtensionss = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArExpandedExtensionss collection (array).
	 *
	 * By default this just sets the collArExpandedExtensionss collection to an empty array (like clearcollArExpandedExtensionss());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArExpandedExtensionss()
	{
		$this->collArExpandedExtensionss = array();
	}

	/**
	 * Gets an array of ArExpandedExtensions objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit has previously been saved, it will retrieve
	 * related ArExpandedExtensionss from storage. If this ArOrganizationUnit is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArExpandedExtensions[]
	 * @throws     PropelException
	 */
	public function getArExpandedExtensionss($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArExpandedExtensionss === null) {
			if ($this->isNew()) {
			   $this->collArExpandedExtensionss = array();
			} else {

				$criteria->add(ArExpandedExtensionsPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArExpandedExtensionsPeer::addSelectColumns($criteria);
				$this->collArExpandedExtensionss = ArExpandedExtensionsPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArExpandedExtensionsPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArExpandedExtensionsPeer::addSelectColumns($criteria);
				if (!isset($this->lastArExpandedExtensionsCriteria) || !$this->lastArExpandedExtensionsCriteria->equals($criteria)) {
					$this->collArExpandedExtensionss = ArExpandedExtensionsPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArExpandedExtensionsCriteria = $criteria;
		return $this->collArExpandedExtensionss;
	}

	/**
	 * Returns the number of related ArExpandedExtensions objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArExpandedExtensions objects.
	 * @throws     PropelException
	 */
	public function countArExpandedExtensionss(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArExpandedExtensionss === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArExpandedExtensionsPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$count = ArExpandedExtensionsPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArExpandedExtensionsPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				if (!isset($this->lastArExpandedExtensionsCriteria) || !$this->lastArExpandedExtensionsCriteria->equals($criteria)) {
					$count = ArExpandedExtensionsPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArExpandedExtensionss);
				}
			} else {
				$count = count($this->collArExpandedExtensionss);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArExpandedExtensions object to this object
	 * through the ArExpandedExtensions foreign key attribute.
	 *
	 * @param      ArExpandedExtensions $l ArExpandedExtensions
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArExpandedExtensions(ArExpandedExtensions $l)
	{
		if ($this->collArExpandedExtensionss === null) {
			$this->initArExpandedExtensionss();
		}
		if (!in_array($l, $this->collArExpandedExtensionss, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArExpandedExtensionss, $l);
			$l->setArOrganizationUnit($this);
		}
	}

	/**
	 * Clears out the collArUsers collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArUsers()
	 */
	public function clearArUsers()
	{
		$this->collArUsers = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArUsers collection (array).
	 *
	 * By default this just sets the collArUsers collection to an empty array (like clearcollArUsers());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArUsers()
	{
		$this->collArUsers = array();
	}

	/**
	 * Gets an array of ArUser objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit has previously been saved, it will retrieve
	 * related ArUsers from storage. If this ArOrganizationUnit is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArUser[]
	 * @throws     PropelException
	 */
	public function getArUsers($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArUsers === null) {
			if ($this->isNew()) {
			   $this->collArUsers = array();
			} else {

				$criteria->add(ArUserPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArUserPeer::addSelectColumns($criteria);
				$this->collArUsers = ArUserPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArUserPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArUserPeer::addSelectColumns($criteria);
				if (!isset($this->lastArUserCriteria) || !$this->lastArUserCriteria->equals($criteria)) {
					$this->collArUsers = ArUserPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArUserCriteria = $criteria;
		return $this->collArUsers;
	}

	/**
	 * Returns the number of related ArUser objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArUser objects.
	 * @throws     PropelException
	 */
	public function countArUsers(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArUsers === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArUserPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$count = ArUserPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArUserPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				if (!isset($this->lastArUserCriteria) || !$this->lastArUserCriteria->equals($criteria)) {
					$count = ArUserPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArUsers);
				}
			} else {
				$count = count($this->collArUsers);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArUser object to this object
	 * through the ArUser foreign key attribute.
	 *
	 * @param      ArUser $l ArUser
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArUser(ArUser $l)
	{
		if ($this->collArUsers === null) {
			$this->initArUsers();
		}
		if (!in_array($l, $this->collArUsers, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArUsers, $l);
			$l->setArOrganizationUnit($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArUsers from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArUsersJoinArParty($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArUsers === null) {
			if ($this->isNew()) {
				$this->collArUsers = array();
			} else {

				$criteria->add(ArUserPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArUsers = ArUserPeer::doSelectJoinArParty($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArUserPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArUserCriteria) || !$this->lastArUserCriteria->equals($criteria)) {
				$this->collArUsers = ArUserPeer::doSelectJoinArParty($criteria, $con, $join_behavior);
			}
		}
		$this->lastArUserCriteria = $criteria;

		return $this->collArUsers;
	}

	/**
	 * Clears out the collArReports collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArReports()
	 */
	public function clearArReports()
	{
		$this->collArReports = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArReports collection (array).
	 *
	 * By default this just sets the collArReports collection to an empty array (like clearcollArReports());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArReports()
	{
		$this->collArReports = array();
	}

	/**
	 * Gets an array of ArReport objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit has previously been saved, it will retrieve
	 * related ArReports from storage. If this ArOrganizationUnit is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArReport[]
	 * @throws     PropelException
	 */
	public function getArReports($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReports === null) {
			if ($this->isNew()) {
			   $this->collArReports = array();
			} else {

				$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArReportPeer::addSelectColumns($criteria);
				$this->collArReports = ArReportPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArReportPeer::addSelectColumns($criteria);
				if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
					$this->collArReports = ArReportPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArReportCriteria = $criteria;
		return $this->collArReports;
	}

	/**
	 * Returns the number of related ArReport objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArReport objects.
	 * @throws     PropelException
	 */
	public function countArReports(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArReports === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$count = ArReportPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
					$count = ArReportPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArReports);
				}
			} else {
				$count = count($this->collArReports);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArReport object to this object
	 * through the ArReport foreign key attribute.
	 *
	 * @param      ArReport $l ArReport
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArReport(ArReport $l)
	{
		if ($this->collArReports === null) {
			$this->initArReports();
		}
		if (!in_array($l, $this->collArReports, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArReports, $l);
			$l->setArOrganizationUnit($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArReportsJoinArReportSetRelatedByArReportSetId($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReports === null) {
			if ($this->isNew()) {
				$this->collArReports = array();
			} else {

				$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArReports = ArReportPeer::doSelectJoinArReportSetRelatedByArReportSetId($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
				$this->collArReports = ArReportPeer::doSelectJoinArReportSetRelatedByArReportSetId($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportCriteria = $criteria;

		return $this->collArReports;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArReportsJoinArReportSetRelatedByAboutArReportSetId($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReports === null) {
			if ($this->isNew()) {
				$this->collArReports = array();
			} else {

				$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArReports = ArReportPeer::doSelectJoinArReportSetRelatedByAboutArReportSetId($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
				$this->collArReports = ArReportPeer::doSelectJoinArReportSetRelatedByAboutArReportSetId($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportCriteria = $criteria;

		return $this->collArReports;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArReportsJoinArUser($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReports === null) {
			if ($this->isNew()) {
				$this->collArReports = array();
			} else {

				$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArReports = ArReportPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
				$this->collArReports = ArReportPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportCriteria = $criteria;

		return $this->collArReports;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArReportsJoinArVendor($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReports === null) {
			if ($this->isNew()) {
				$this->collArReports = array();
			} else {

				$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArReports = ArReportPeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
				$this->collArReports = ArReportPeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportCriteria = $criteria;

		return $this->collArReports;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArReportsJoinArTag($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReports === null) {
			if ($this->isNew()) {
				$this->collArReports = array();
			} else {

				$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArReports = ArReportPeer::doSelectJoinArTag($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
				$this->collArReports = ArReportPeer::doSelectJoinArTag($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportCriteria = $criteria;

		return $this->collArReports;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArReportsJoinArReportOrderOfChildren($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReports === null) {
			if ($this->isNew()) {
				$this->collArReports = array();
			} else {

				$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArReports = ArReportPeer::doSelectJoinArReportOrderOfChildren($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
				$this->collArReports = ArReportPeer::doSelectJoinArReportOrderOfChildren($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportCriteria = $criteria;

		return $this->collArReports;
	}

	/**
	 * Clears out the collArReportSchedulers collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArReportSchedulers()
	 */
	public function clearArReportSchedulers()
	{
		$this->collArReportSchedulers = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArReportSchedulers collection (array).
	 *
	 * By default this just sets the collArReportSchedulers collection to an empty array (like clearcollArReportSchedulers());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArReportSchedulers()
	{
		$this->collArReportSchedulers = array();
	}

	/**
	 * Gets an array of ArReportScheduler objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit has previously been saved, it will retrieve
	 * related ArReportSchedulers from storage. If this ArOrganizationUnit is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArReportScheduler[]
	 * @throws     PropelException
	 */
	public function getArReportSchedulers($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportSchedulers === null) {
			if ($this->isNew()) {
			   $this->collArReportSchedulers = array();
			} else {

				$criteria->add(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArReportSchedulerPeer::addSelectColumns($criteria);
				$this->collArReportSchedulers = ArReportSchedulerPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArReportSchedulerPeer::addSelectColumns($criteria);
				if (!isset($this->lastArReportSchedulerCriteria) || !$this->lastArReportSchedulerCriteria->equals($criteria)) {
					$this->collArReportSchedulers = ArReportSchedulerPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArReportSchedulerCriteria = $criteria;
		return $this->collArReportSchedulers;
	}

	/**
	 * Returns the number of related ArReportScheduler objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArReportScheduler objects.
	 * @throws     PropelException
	 */
	public function countArReportSchedulers(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArReportSchedulers === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$count = ArReportSchedulerPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				if (!isset($this->lastArReportSchedulerCriteria) || !$this->lastArReportSchedulerCriteria->equals($criteria)) {
					$count = ArReportSchedulerPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArReportSchedulers);
				}
			} else {
				$count = count($this->collArReportSchedulers);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArReportScheduler object to this object
	 * through the ArReportScheduler foreign key attribute.
	 *
	 * @param      ArReportScheduler $l ArReportScheduler
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArReportScheduler(ArReportScheduler $l)
	{
		if ($this->collArReportSchedulers === null) {
			$this->initArReportSchedulers();
		}
		if (!in_array($l, $this->collArReportSchedulers, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArReportSchedulers, $l);
			$l->setArOrganizationUnit($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArReportSchedulers from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArReportSchedulersJoinArReport($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportSchedulers === null) {
			if ($this->isNew()) {
				$this->collArReportSchedulers = array();
			} else {

				$criteria->add(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArReportSchedulers = ArReportSchedulerPeer::doSelectJoinArReport($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArReportSchedulerCriteria) || !$this->lastArReportSchedulerCriteria->equals($criteria)) {
				$this->collArReportSchedulers = ArReportSchedulerPeer::doSelectJoinArReport($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportSchedulerCriteria = $criteria;

		return $this->collArReportSchedulers;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArReportSchedulers from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArReportSchedulersJoinArReportGeneration($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportSchedulers === null) {
			if ($this->isNew()) {
				$this->collArReportSchedulers = array();
			} else {

				$criteria->add(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArReportSchedulers = ArReportSchedulerPeer::doSelectJoinArReportGeneration($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArReportSchedulerCriteria) || !$this->lastArReportSchedulerCriteria->equals($criteria)) {
				$this->collArReportSchedulers = ArReportSchedulerPeer::doSelectJoinArReportGeneration($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportSchedulerCriteria = $criteria;

		return $this->collArReportSchedulers;
	}

	/**
	 * Clears out the collArPostponedReports collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArPostponedReports()
	 */
	public function clearArPostponedReports()
	{
		$this->collArPostponedReports = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArPostponedReports collection (array).
	 *
	 * By default this just sets the collArPostponedReports collection to an empty array (like clearcollArPostponedReports());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArPostponedReports()
	{
		$this->collArPostponedReports = array();
	}

	/**
	 * Gets an array of ArPostponedReport objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit has previously been saved, it will retrieve
	 * related ArPostponedReports from storage. If this ArOrganizationUnit is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArPostponedReport[]
	 * @throws     PropelException
	 */
	public function getArPostponedReports($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArPostponedReports === null) {
			if ($this->isNew()) {
			   $this->collArPostponedReports = array();
			} else {

				$criteria->add(ArPostponedReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArPostponedReportPeer::addSelectColumns($criteria);
				$this->collArPostponedReports = ArPostponedReportPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArPostponedReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArPostponedReportPeer::addSelectColumns($criteria);
				if (!isset($this->lastArPostponedReportCriteria) || !$this->lastArPostponedReportCriteria->equals($criteria)) {
					$this->collArPostponedReports = ArPostponedReportPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArPostponedReportCriteria = $criteria;
		return $this->collArPostponedReports;
	}

	/**
	 * Returns the number of related ArPostponedReport objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArPostponedReport objects.
	 * @throws     PropelException
	 */
	public function countArPostponedReports(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArPostponedReports === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArPostponedReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$count = ArPostponedReportPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArPostponedReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				if (!isset($this->lastArPostponedReportCriteria) || !$this->lastArPostponedReportCriteria->equals($criteria)) {
					$count = ArPostponedReportPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArPostponedReports);
				}
			} else {
				$count = count($this->collArPostponedReports);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArPostponedReport object to this object
	 * through the ArPostponedReport foreign key attribute.
	 *
	 * @param      ArPostponedReport $l ArPostponedReport
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArPostponedReport(ArPostponedReport $l)
	{
		if ($this->collArPostponedReports === null) {
			$this->initArPostponedReports();
		}
		if (!in_array($l, $this->collArPostponedReports, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArPostponedReports, $l);
			$l->setArOrganizationUnit($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArPostponedReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArPostponedReportsJoinArReportSet($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArPostponedReports === null) {
			if ($this->isNew()) {
				$this->collArPostponedReports = array();
			} else {

				$criteria->add(ArPostponedReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArPostponedReports = ArPostponedReportPeer::doSelectJoinArReportSet($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArPostponedReportPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArPostponedReportCriteria) || !$this->lastArPostponedReportCriteria->equals($criteria)) {
				$this->collArPostponedReports = ArPostponedReportPeer::doSelectJoinArReportSet($criteria, $con, $join_behavior);
			}
		}
		$this->lastArPostponedReportCriteria = $criteria;

		return $this->collArPostponedReports;
	}

	/**
	 * Gets a single ArPostponedReportTmp object, which is related to this object by a one-to-one relationship.
	 *
	 * @param      PropelPDO $con
	 * @return     ArPostponedReportTmp
	 * @throws     PropelException
	 */
	public function getArPostponedReportTmp(PropelPDO $con = null)
	{

		if ($this->singleArPostponedReportTmp === null && !$this->isNew()) {
			$this->singleArPostponedReportTmp = ArPostponedReportTmpPeer::retrieveByPK($this->id, $con);
		}

		return $this->singleArPostponedReportTmp;
	}

	/**
	 * Sets a single ArPostponedReportTmp object as related to this object by a one-to-one relationship.
	 *
	 * @param      ArPostponedReportTmp $l ArPostponedReportTmp
	 * @return     ArOrganizationUnit The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArPostponedReportTmp(ArPostponedReportTmp $v)
	{
		$this->singleArPostponedReportTmp = $v;

		// Make sure that that the passed-in ArPostponedReportTmp isn't already associated with this object
		if ($v->getArOrganizationUnit() === null) {
			$v->setArOrganizationUnit($this);
		}

		return $this;
	}

	/**
	 * Clears out the collArReportToReadUserViews collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArReportToReadUserViews()
	 */
	public function clearArReportToReadUserViews()
	{
		$this->collArReportToReadUserViews = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArReportToReadUserViews collection (array).
	 *
	 * By default this just sets the collArReportToReadUserViews collection to an empty array (like clearcollArReportToReadUserViews());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArReportToReadUserViews()
	{
		$this->collArReportToReadUserViews = array();
	}

	/**
	 * Gets an array of ArReportToReadUserView objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit has previously been saved, it will retrieve
	 * related ArReportToReadUserViews from storage. If this ArOrganizationUnit is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArReportToReadUserView[]
	 * @throws     PropelException
	 */
	public function getArReportToReadUserViews($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
			   $this->collArReportToReadUserViews = array();
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArReportToReadUserViewPeer::addSelectColumns($criteria);
				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArReportToReadUserViewPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArReportToReadUserViewPeer::addSelectColumns($criteria);
				if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
					$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArReportToReadUserViewCriteria = $criteria;
		return $this->collArReportToReadUserViews;
	}

	/**
	 * Returns the number of related ArReportToReadUserView objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArReportToReadUserView objects.
	 * @throws     PropelException
	 */
	public function countArReportToReadUserViews(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$count = ArReportToReadUserViewPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArReportToReadUserViewPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
					$count = ArReportToReadUserViewPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArReportToReadUserViews);
				}
			} else {
				$count = count($this->collArReportToReadUserViews);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArReportToReadUserView object to this object
	 * through the ArReportToReadUserView foreign key attribute.
	 *
	 * @param      ArReportToReadUserView $l ArReportToReadUserView
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArReportToReadUserView(ArReportToReadUserView $l)
	{
		if ($this->collArReportToReadUserViews === null) {
			$this->initArReportToReadUserViews();
		}
		if (!in_array($l, $this->collArReportToReadUserViews, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArReportToReadUserViews, $l);
			$l->setArOrganizationUnit($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArReportToReadUserViews from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArReportToReadUserViewsJoinArReportToRead($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
				$this->collArReportToReadUserViews = array();
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArReportToRead($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportToReadUserViewPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArReportToRead($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportToReadUserViewCriteria = $criteria;

		return $this->collArReportToReadUserViews;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArReportToReadUserViews from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArReportToReadUserViewsJoinArReport($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
				$this->collArReportToReadUserViews = array();
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArReport($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportToReadUserViewPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArReport($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportToReadUserViewCriteria = $criteria;

		return $this->collArReportToReadUserViews;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArReportToReadUserViews from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArReportToReadUserViewsJoinArUser($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
				$this->collArReportToReadUserViews = array();
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportToReadUserViewPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportToReadUserViewCriteria = $criteria;

		return $this->collArReportToReadUserViews;
	}

	/**
	 * Clears out the collArInstanceStatuss collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArInstanceStatuss()
	 */
	public function clearArInstanceStatuss()
	{
		$this->collArInstanceStatuss = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArInstanceStatuss collection (array).
	 *
	 * By default this just sets the collArInstanceStatuss collection to an empty array (like clearcollArInstanceStatuss());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArInstanceStatuss()
	{
		$this->collArInstanceStatuss = array();
	}

	/**
	 * Gets an array of ArInstanceStatus objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit has previously been saved, it will retrieve
	 * related ArInstanceStatuss from storage. If this ArOrganizationUnit is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArInstanceStatus[]
	 * @throws     PropelException
	 */
	public function getArInstanceStatuss($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArInstanceStatuss === null) {
			if ($this->isNew()) {
			   $this->collArInstanceStatuss = array();
			} else {

				$criteria->add(ArInstanceStatusPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArInstanceStatusPeer::addSelectColumns($criteria);
				$this->collArInstanceStatuss = ArInstanceStatusPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArInstanceStatusPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArInstanceStatusPeer::addSelectColumns($criteria);
				if (!isset($this->lastArInstanceStatusCriteria) || !$this->lastArInstanceStatusCriteria->equals($criteria)) {
					$this->collArInstanceStatuss = ArInstanceStatusPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArInstanceStatusCriteria = $criteria;
		return $this->collArInstanceStatuss;
	}

	/**
	 * Returns the number of related ArInstanceStatus objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArInstanceStatus objects.
	 * @throws     PropelException
	 */
	public function countArInstanceStatuss(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArInstanceStatuss === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArInstanceStatusPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$count = ArInstanceStatusPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArInstanceStatusPeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				if (!isset($this->lastArInstanceStatusCriteria) || !$this->lastArInstanceStatusCriteria->equals($criteria)) {
					$count = ArInstanceStatusPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArInstanceStatuss);
				}
			} else {
				$count = count($this->collArInstanceStatuss);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArInstanceStatus object to this object
	 * through the ArInstanceStatus foreign key attribute.
	 *
	 * @param      ArInstanceStatus $l ArInstanceStatus
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArInstanceStatus(ArInstanceStatus $l)
	{
		if ($this->collArInstanceStatuss === null) {
			$this->initArInstanceStatuss();
		}
		if (!in_array($l, $this->collArInstanceStatuss, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArInstanceStatuss, $l);
			$l->setArOrganizationUnit($this);
		}
	}

	/**
	 * Clears out the collArAssignedServices collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArAssignedServices()
	 */
	public function clearArAssignedServices()
	{
		$this->collArAssignedServices = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArAssignedServices collection (array).
	 *
	 * By default this just sets the collArAssignedServices collection to an empty array (like clearcollArAssignedServices());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArAssignedServices()
	{
		$this->collArAssignedServices = array();
	}

	/**
	 * Gets an array of ArAssignedService objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit has previously been saved, it will retrieve
	 * related ArAssignedServices from storage. If this ArOrganizationUnit is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArAssignedService[]
	 * @throws     PropelException
	 */
	public function getArAssignedServices($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArAssignedServices === null) {
			if ($this->isNew()) {
			   $this->collArAssignedServices = array();
			} else {

				$criteria->add(ArAssignedServicePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArAssignedServicePeer::addSelectColumns($criteria);
				$this->collArAssignedServices = ArAssignedServicePeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArAssignedServicePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				ArAssignedServicePeer::addSelectColumns($criteria);
				if (!isset($this->lastArAssignedServiceCriteria) || !$this->lastArAssignedServiceCriteria->equals($criteria)) {
					$this->collArAssignedServices = ArAssignedServicePeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArAssignedServiceCriteria = $criteria;
		return $this->collArAssignedServices;
	}

	/**
	 * Returns the number of related ArAssignedService objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArAssignedService objects.
	 * @throws     PropelException
	 */
	public function countArAssignedServices(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArAssignedServices === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArAssignedServicePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$count = ArAssignedServicePeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArAssignedServicePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				if (!isset($this->lastArAssignedServiceCriteria) || !$this->lastArAssignedServiceCriteria->equals($criteria)) {
					$count = ArAssignedServicePeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArAssignedServices);
				}
			} else {
				$count = count($this->collArAssignedServices);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArAssignedService object to this object
	 * through the ArAssignedService foreign key attribute.
	 *
	 * @param      ArAssignedService $l ArAssignedService
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArAssignedService(ArAssignedService $l)
	{
		if ($this->collArAssignedServices === null) {
			$this->initArAssignedServices();
		}
		if (!in_array($l, $this->collArAssignedServices, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArAssignedServices, $l);
			$l->setArOrganizationUnit($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArOrganizationUnit is new, it will return
	 * an empty collection; or if this ArOrganizationUnit has previously
	 * been saved, it will retrieve related ArAssignedServices from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArOrganizationUnit.
	 */
	public function getArAssignedServicesJoinArService($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArOrganizationUnitPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArAssignedServices === null) {
			if ($this->isNew()) {
				$this->collArAssignedServices = array();
			} else {

				$criteria->add(ArAssignedServicePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

				$this->collArAssignedServices = ArAssignedServicePeer::doSelectJoinArService($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArAssignedServicePeer::AR_ORGANIZATION_UNIT_ID, $this->id);

			if (!isset($this->lastArAssignedServiceCriteria) || !$this->lastArAssignedServiceCriteria->equals($criteria)) {
				$this->collArAssignedServices = ArAssignedServicePeer::doSelectJoinArService($criteria, $con, $join_behavior);
			}
		}
		$this->lastArAssignedServiceCriteria = $criteria;

		return $this->collArAssignedServices;
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
			if ($this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId) {
				foreach ((array) $this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId) {
				foreach ((array) $this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArExpandedExtensionss) {
				foreach ((array) $this->collArExpandedExtensionss as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArUsers) {
				foreach ((array) $this->collArUsers as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArReports) {
				foreach ((array) $this->collArReports as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArReportSchedulers) {
				foreach ((array) $this->collArReportSchedulers as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArPostponedReports) {
				foreach ((array) $this->collArPostponedReports as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->singleArPostponedReportTmp) {
				$this->singleArPostponedReportTmp->clearAllReferences($deep);
			}
			if ($this->collArReportToReadUserViews) {
				foreach ((array) $this->collArReportToReadUserViews as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArInstanceStatuss) {
				foreach ((array) $this->collArInstanceStatuss as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArAssignedServices) {
				foreach ((array) $this->collArAssignedServices as $o) {
					$o->clearAllReferences($deep);
				}
			}
		} // if ($deep)

		$this->collArCdrs = null;
		$this->collArOrganizationUnitHasStructuresRelatedByArOrganizationUnitId = null;
		$this->collArOrganizationUnitHasStructuresRelatedByArParentOrganizationUnitId = null;
		$this->collArExpandedExtensionss = null;
		$this->collArUsers = null;
		$this->collArReports = null;
		$this->collArReportSchedulers = null;
		$this->collArPostponedReports = null;
		$this->singleArPostponedReportTmp = null;
		$this->collArReportToReadUserViews = null;
		$this->collArInstanceStatuss = null;
		$this->collArAssignedServices = null;
	}

} // BaseArOrganizationUnit
