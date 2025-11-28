-- 1. CREACIÓN DE BASE DE DATOS Y USO

IF DB_ID('PanaderiaDB') IS NULL
 BEGIN
    CREATE DATABASE PanaderiaDB;
 END;
GO -- El GO debe estar en su propia línea
USE PanaderiaDB;
GO


-- 2. DDL - Definición de Estructura de Tablas
-- (Uso de NVARCHAR para texto y ajustes en Pedidos/Estados)


-- 2.1. Tabla de Usuarios
CREATE TABLE Usuarios (
    IdUsuario       INT PRIMARY KEY IDENTITY(1,1),
    NombreCompleto  NVARCHAR(100) NOT NULL,
    Telefono        NVARCHAR(20) NULL,
    Correo          NVARCHAR(100) NOT NULL UNIQUE,
    Contrasena      VARCHAR(100) NOT NULL,
    Rol             NVARCHAR(20) NOT NULL DEFAULT 'Cliente',
    Activo          BIT NOT NULL DEFAULT 1,
    FechaRegistro   DATETIME NOT NULL DEFAULT GETDATE(),
    CONSTRAINT CK_Usuarios_Rol CHECK (Rol IN ('Administrador', 'Cliente'))
 );
 
-- 2.2. Tabla de Categorías
CREATE TABLE Categorias (
    IdCategoria INT PRIMARY KEY IDENTITY(1,1),
    Nombre      NVARCHAR(50) NOT NULL,
    Descripcion NVARCHAR(100) NULL,
    Activo      BIT NOT NULL DEFAULT 1
 );
 
-- 2.3. Tabla de Productos
CREATE TABLE Productos (
    IdProducto   INT PRIMARY KEY IDENTITY(1,1),
    IdCategoria  INT NOT NULL,
    Nombre       NVARCHAR(50) NOT NULL,
    Descripcion  NVARCHAR(200) NULL,
    PrecioVenta  DECIMAL(10,2) NOT NULL,
    ImagenURL    VARCHAR(255) NULL,
    Stock        INT NOT NULL DEFAULT 0,
    Activo       BIT NOT NULL DEFAULT 1,
    CONSTRAINT FK_Productos_Categorias 
        FOREIGN KEY (IdCategoria) REFERENCES Categorias(IdCategoria),
    CONSTRAINT CK_Productos_PrecioVenta CHECK (PrecioVenta >= 0),
    CONSTRAINT CK_Productos_Stock CHECK (Stock >= 0)
 );
 
-- 2.4. Tabla de Ingredientes (Inventario)
CREATE TABLE Ingredientes (
    IdIngrediente INT PRIMARY KEY IDENTITY(1,1),
    Nombre        NVARCHAR(50) NOT NULL,
    Cantidad      DECIMAL(10,2) NOT NULL,
    UnidadMedida  NVARCHAR(20) NOT NULL,
    CostoUnitario DECIMAL(10,2) NULL,
    FechaVencimiento DATE NULL,
    CONSTRAINT CK_Ingredientes_Cantidad CHECK (Cantidad >= 0),
    CONSTRAINT CK_Ingredientes_CostoUnitario 
        CHECK (CostoUnitario IS NULL OR CostoUnitario >= 0)
 );
 
-- 2.5. Tabla de Recetas (Cabecera)
CREATE TABLE Recetas (
    IdReceta      INT PRIMARY KEY IDENTITY(1,1),
    -- **CORRECCIÓN: Se mantiene UNIQUE (1 producto = 1 receta) según tu diseño original**
    IdProducto    INT NOT NULL UNIQUE, 
    FechaCreacion DATETIME NOT NULL DEFAULT GETDATE(),
    CONSTRAINT FK_Recetas_Productos 
        FOREIGN KEY (IdProducto) REFERENCES Productos(IdProducto)
 );
 
-- 2.6. Tabla Detalle de Recetas
CREATE TABLE DetalleRecetas (
    IdDetalleReceta   INT PRIMARY KEY IDENTITY(1,1),
    IdReceta          INT NOT NULL,
    IdIngrediente     INT NOT NULL,
    CantidadRequerida DECIMAL(10,4) NOT NULL,
    CONSTRAINT FK_DetalleRecetas_Recetas 
        FOREIGN KEY (IdReceta) REFERENCES Recetas(IdReceta),
    CONSTRAINT FK_DetalleRecetas_Ingredientes 
        FOREIGN KEY (IdIngrediente) REFERENCES Ingredientes(IdIngrediente),
    CONSTRAINT UQ_DetalleRecetas UNIQUE (IdReceta, IdIngrediente),
    CONSTRAINT CK_DetalleRecetas_Cantidad CHECK (CantidadRequerida > 0)
 );
 
-- 2.7. Registro de Producción (log de horneadas)
CREATE TABLE RegistroProduccion (
    IdRegistro        INT PRIMARY KEY IDENTITY(1,1),
    IdProducto        INT NOT NULL,
    CantidadProducida INT NOT NULL,
    FechaProduccion   DATETIME NOT NULL DEFAULT GETDATE(),
    Observaciones     NVARCHAR(500) NULL,
    CONSTRAINT FK_RegistroProduccion_Productos 
        FOREIGN KEY (IdProducto) REFERENCES Productos(IdProducto),
    CONSTRAINT CK_RegistroProduccion_Cantidad CHECK (CantidadProducida > 0)
 );
 
-- 2.8. Tabla de Pedidos (Cabecera)
CREATE TABLE Pedidos (
    IdPedido        INT PRIMARY KEY IDENTITY(1,1),
    IdUsuario       INT NOT NULL,
    FechaPedido     DATETIME NOT NULL DEFAULT GETDATE(),
    DireccionEntrega NVARCHAR(255) NULL,
    Observaciones   NVARCHAR(500) NULL,
    MetodoPago      NVARCHAR(50) NOT NULL,
    Total           DECIMAL(10,2) NOT NULL,
    Estado          NVARCHAR(20) NOT NULL DEFAULT 'Pendiente',
    CONSTRAINT FK_Pedidos_Usuarios 
        FOREIGN KEY (IdUsuario) REFERENCES Usuarios(IdUsuario),
    CONSTRAINT CK_Pedidos_Total CHECK (Total >= 0),
    CONSTRAINT CK_Pedidos_Estado 
        CHECK (Estado IN ('Pendiente','En Preparacion','Completado','Cancelado'))
 );
 
-- 2.9. Detalle de Pedidos
CREATE TABLE DetallePedidos (
    IdDetalle      INT PRIMARY KEY IDENTITY(1,1),
    IdPedido       INT NOT NULL,
    IdProducto     INT NOT NULL,
    Cantidad       INT NOT NULL,
    PrecioUnitario DECIMAL(10,2) NOT NULL,
    CONSTRAINT FK_DetallePedidos_Pedidos 
        FOREIGN KEY (IdPedido)REFERENCES Pedidos(IdPedido),
    CONSTRAINT FK_DetallePedidos_Productos 
        FOREIGN KEY (IdProducto) REFERENCES Productos(IdProducto),
    CONSTRAINT CK_DetallePedidos_Cantidad CHECK (Cantidad > 0),
    CONSTRAINT CK_DetallePedidos_Precio CHECK (PrecioUnitario >= 0),
    CONSTRAINT UQ_DetallePedidos UNIQUE (IdPedido, IdProducto)
 );
GO

--- 3. SPs de Producción y Venta (Gestión de Stock)

-- 3.1. Registrar Producción (Descarga ingredientes, aumenta stock)
CREATE PROCEDURE SP_RegistrarProduccion
    @IdProducto       INT,
    @CantidadProducida INT,
    @Observaciones    NVARCHAR(500) = NULL
AS
BEGIN
    SET NOCOUNT ON;
 
    -- Validaciones
    IF @IdProducto IS NULL OR @IdProducto <= 0
        RETURN;
    IF @CantidadProducida IS NULL OR @CantidadProducida <= 0
        RETURN;
    IF NOT EXISTS (SELECT 1 FROM Productos WHERE IdProducto = @IdProducto AND Activo = 1)
        RETURN;
    IF NOT EXISTS (SELECT 1 FROM Recetas WHERE IdProducto = @IdProducto)
        RETURN;
 
    -- **CORRECCIÓN: Obtener @IdReceta antes de la transacción**
    DECLARE @IdReceta INT;
    SELECT @IdReceta = IdReceta FROM Recetas WHERE IdProducto = @IdProducto;
 
    BEGIN TRANSACTION;
    BEGIN TRY
 
        -- PASO 1: Calcular los requerimientos totales (SET-BASED)
        SELECT 
            dr.IdIngrediente,
            dr.CantidadRequerida * @CantidadProducida AS CantidadConsumir
        INTO 
            #Requerimientos
        FROM 
            DetalleRecetas dr
        WHERE 
            dr.IdReceta = @IdReceta;
 
        -- PASO 2: Validación de Stock Insuficiente
        IF EXISTS (
            SELECT 1
            FROM #Requerimientos req
            JOIN Ingredientes i WITH (UPDLOCK) ON req.IdIngrediente = i.IdIngrediente
            WHERE i.Cantidad < req.CantidadConsumir
        )
        BEGIN
            RAISERROR('Stock insuficiente de uno o más ingredientes para la producción solicitada.', 16, 1);
            -- Se lanza error para ser capturado por el CATCH y hacer ROLLBACK
            -- Se añade un RETURN para salir del TRY en caso de error
            RETURN;
        END;
 
        -- PASO 3: Descontar Inventario de Ingredientes (SET-BASED)
        UPDATE i
        SET i.Cantidad = i.Cantidad - req.CantidadConsumir
        FROM Ingredientes i
        JOIN #Requerimientos req ON i.IdIngrediente = req.IdIngrediente;
 
        -- PASO 4: Aumentar stock de producto terminado
        UPDATE Productos
          SET Stock = Stock + @CantidadProducida
        WHERE IdProducto = @IdProducto;
 
        -- PASO 5: Registrar en log de producción
        INSERT INTO RegistroProduccion (IdProducto, CantidadProducida, FechaProduccion, Observaciones)
        VALUES (@IdProducto, @CantidadProducida, GETDATE(), @Observaciones);
 
        -- Limpieza
        DROP TABLE #Requerimientos;
 
        COMMIT TRANSACTION;
        SELECT 1 AS Resultado, 'Producción registrada y stock actualizado correctamente.' AS Mensaje;
 
    END TRY
    BEGIN CATCH
        IF OBJECT_ID('tempdb..#Requerimientos') IS NOT NULL
            DROP TABLE #Requerimientos;
 
        IF @@TRANCOUNT > 0
            ROLLBACK TRANSACTION;
 
        SELECT 0 AS Resultado, 'Fallo en la Producción: ' + ERROR_MESSAGE() AS Mensaje;
    END CATCH
END;
GO
 
-- 3.2. Registrar Venta (Descuento de stock del producto terminado)
CREATE PROCEDURE SP_RegistrarVenta
    @IdProducto     INT,
    @CantidadVendida INT
AS
BEGIN
 
    SET NOCOUNT ON;
 
    IF @IdProducto IS NULL OR @IdProducto <= 0
        RETURN;
    IF @CantidadVendida IS NULL OR @CantidadVendida <= 0
        RETURN;
    IF NOT EXISTS (SELECT 1 FROM Productos WHERE IdProducto = @IdProducto AND Activo = 1)
        RETURN;
 
    BEGIN TRANSACTION;
    BEGIN TRY
        DECLARE @StockActual INT;
 
        SELECT @StockActual = Stock
        FROM Productos WITH (UPDLOCK)
        WHERE IdProducto = @IdProducto;
 
        IF @StockActual < @CantidadVendida
        BEGIN
            RAISERROR('Stock de producto terminado insuficiente para la venta.', 16, 1);
            -- Se lanza error para ser capturado por el CATCH y hacer ROLLBACK
            RETURN;
        END;
 
        UPDATE Productos
        SET Stock = Stock - @CantidadVendida
        WHERE IdProducto = @IdProducto;
 
        COMMIT TRANSACTION;
        SELECT 1 AS Resultado, 'Venta descontada de stock correctamente.' AS Mensaje;
    END TRY
    BEGIN CATCH
        IF @@TRANCOUNT > 0
            ROLLBACK TRANSACTION;
        SELECT 0 AS Resultado, 'Fallo al registrar la venta: ' + ERROR_MESSAGE() AS Mensaje;
    END CATCH
END;
GO

--- 4. SPs de Gestión de Productos

-- 4.1. Listar Productos
CREATE PROCEDURE SP_ListarProductos
AS
BEGIN
    SET NOCOUNT ON;
    
    SELECT 
        p.IdProducto,
        p.IdCategoria,
        c.Nombre AS NombreCategoria,
        p.Nombre,
        p.Descripcion,
        p.PrecioVenta,
        p.ImagenURL,
        p.Stock,
        p.Activo
    FROM 
        Productos p
    JOIN 
        Categorias c ON p.IdCategoria = c.IdCategoria
    ORDER BY 
        p.Nombre;
END;
GO

-- 4.2. Insertar Producto
CREATE PROCEDURE SP_InsertarProducto
    @IdCategoria INT,
    @Nombre NVARCHAR(50), 
    @Descripcion NVARCHAR(200) = NULL,
    @PrecioVenta DECIMAL(10,2),
    @ImagenURL VARCHAR(255) = NULL,
    @Stock INT = 0
AS
BEGIN
    SET NOCOUNT ON;

    IF @IdCategoria IS NULL OR @IdCategoria <= 0 OR NOT EXISTS (SELECT 1 FROM Categorias WHERE IdCategoria = @IdCategoria AND Activo = 1)
        RETURN;

    IF @Nombre IS NULL OR LTRIM(RTRIM(@Nombre)) = ''
        RETURN;

    IF @PrecioVenta IS NULL OR @PrecioVenta < 0
        RETURN;

    BEGIN TRY
        INSERT INTO Productos (IdCategoria, Nombre, Descripcion, PrecioVenta, ImagenURL, Stock, Activo)
        VALUES (@IdCategoria, @Nombre, @Descripcion, @PrecioVenta, @ImagenURL, @Stock, 1);

        SELECT 1 AS Resultado, 'Producto insertado correctamente.' AS Mensaje, SCOPE_IDENTITY() AS IdGenerado;
    END TRY
    BEGIN CATCH
        SELECT 0 AS Resultado, 'Fallo al insertar el producto: ' + ERROR_MESSAGE() AS Mensaje, NULL AS IdGenerado;
    END CATCH
END;
GO

-- 4.3. Actualizar Producto
CREATE PROCEDURE SP_ActualizarProducto
    @IdProducto INT,
    @IdCategoria INT,
    @Nombre NVARCHAR(50),
    @Descripcion NVARCHAR(200) = NULL,
    @PrecioVenta DECIMAL(10,2),
    @ImagenURL VARCHAR(255) = NULL,
    @Activo BIT
AS
BEGIN
    SET NOCOUNT ON;

    IF @IdProducto IS NULL OR @IdProducto <= 0 OR NOT EXISTS (SELECT 1 FROM Productos WHERE IdProducto = @IdProducto)
        RETURN;

    IF @IdCategoria IS NULL OR @IdCategoria <= 0 OR NOT EXISTS (SELECT 1 FROM Categorias WHERE IdCategoria = @IdCategoria)
        RETURN;

    IF @Nombre IS NULL OR LTRIM(RTRIM(@Nombre)) = ''
        RETURN;

    IF @PrecioVenta IS NULL OR @PrecioVenta < 0
        RETURN;

    BEGIN TRY
        UPDATE Productos
        SET 
            IdCategoria = @IdCategoria,
            Nombre = @Nombre,
            Descripcion = @Descripcion,
            PrecioVenta = @PrecioVenta,
            ImagenURL = @ImagenURL,
            Activo = @Activo
        WHERE 
            IdProducto = @IdProducto;

        SELECT 1 AS Resultado, 'Producto actualizado correctamente.' AS Mensaje;
    END TRY
    BEGIN CATCH
        SELECT 0 AS Resultado, 'Fallo al actualizar el producto: ' + ERROR_MESSAGE() AS Mensaje;
    END CATCH
END;
GO

-- 4.4. Eliminar Producto (Eliminación Lógica)
CREATE PROCEDURE SP_EliminarProducto
    @IdProducto INT
AS
BEGIN
    SET NOCOUNT ON;

    IF @IdProducto IS NULL OR @IdProducto <= 0 OR NOT EXISTS (SELECT 1 FROM Productos WHERE IdProducto = @IdProducto)
        RETURN;

    BEGIN TRY
        UPDATE Productos
        SET Activo = 0 
        WHERE IdProducto = @IdProducto;

        SELECT 1 AS Resultado, 'Producto deshabilitado (eliminación lógica) correctamente.' AS Mensaje;
    END TRY
    BEGIN CATCH
        SELECT 0 AS Resultado, 'Fallo al deshabilitar el producto: ' + ERROR_MESSAGE() AS Mensaje;
    END CATCH
END;
GO

---5. SPs de Gestión de Ingredientes

-- 5.1. Listar ingredientes
CREATE PROCEDURE SP_ListarIngredientes
AS
BEGIN
    SET NOCOUNT ON;
 
    SELECT
        IdIngrediente, Nombre, Cantidad, UnidadMedida, CostoUnitario, FechaVencimiento
    FROM Ingredientes
    ORDER BY Nombre;
END;
GO
 
-- 5.2. Insertar ingrediente
CREATE PROCEDURE SP_InsertarIngrediente
    @Nombre        NVARCHAR(50), 
    @Cantidad      DECIMAL(10,2),
    @UnidadMedida  NVARCHAR(20), 
    @CostoUnitario DECIMAL(10,2),
    @FechaVencimiento DATE = NULL
AS
BEGIN
    SET NOCOUNT ON;
    IF @Nombre IS NULL OR LTRIM(RTRIM(@Nombre)) = '' RETURN;
    IF @Cantidad IS NULL OR @Cantidad < 0 RETURN;
    IF @CostoUnitario IS NOT NULL AND @CostoUnitario < 0 RETURN;
 
    BEGIN TRY
        INSERT INTO Ingredientes (Nombre, Cantidad, UnidadMedida, CostoUnitario, FechaVencimiento)
        VALUES (@Nombre, @Cantidad, @UnidadMedida, @CostoUnitario, @FechaVencimiento);
        SELECT 1 AS Resultado, 'Ingrediente insertado correctamente.' AS Mensaje, SCOPE_IDENTITY() AS IdGenerado;
    END TRY
    BEGIN CATCH
        SELECT 0 AS Resultado, 'Error al insertar el ingrediente: ' + ERROR_MESSAGE() AS Mensaje, NULL AS IdGenerado;
    END CATCH
END;
GO
 
-- 5.3. Actualizar ingrediente
CREATE PROCEDURE SP_ActualizarIngrediente
    @IdIngrediente INT,
    @Nombre        NVARCHAR(50), 
    @Cantidad      DECIMAL(10,2),
    @UnidadMedida  NVARCHAR(20), 
    @CostoUnitario DECIMAL(10,2),
    @FechaVencimiento DATE = NULL
AS
BEGIN
    SET NOCOUNT ON;
    IF @IdIngrediente IS NULL OR @IdIngrediente <= 0 RETURN;
    IF NOT EXISTS (SELECT 1 FROM Ingredientes WHERE IdIngrediente = @IdIngrediente) RETURN;
    IF @Nombre IS NULL OR LTRIM(RTRIM(@Nombre)) = '' RETURN;
    IF @Cantidad IS NULL OR @Cantidad < 0 RETURN;
    IF @CostoUnitario IS NOT NULL AND @CostoUnitario < 0 RETURN;
 
    BEGIN TRY
        UPDATE Ingredientes
        SET Nombre = @Nombre, Cantidad = @Cantidad, UnidadMedida = @UnidadMedida, CostoUnitario = @CostoUnitario, FechaVencimiento = @FechaVencimiento
        WHERE IdIngrediente = @IdIngrediente;
        SELECT 1 AS Resultado, 'Ingrediente actualizado correctamente.' AS Mensaje;
    END TRY
    BEGIN CATCH
        SELECT 0 AS Resultado, 'Error al actualizar el ingrediente: ' + ERROR_MESSAGE() AS Mensaje;
    END CATCH
END;
GO
 
-- 5.4. Eliminar ingrediente
CREATE PROCEDURE SP_EliminarIngrediente
    @IdIngrediente INT
AS
BEGIN
    SET NOCOUNT ON;
    IF @IdIngrediente IS NULL OR @IdIngrediente <= 0 RETURN;
    IF NOT EXISTS (SELECT 1 FROM Ingredientes WHERE IdIngrediente = @IdIngrediente) RETURN;
 
    IF EXISTS (SELECT 1 FROM DetalleRecetas WHERE IdIngrediente = @IdIngrediente)
    BEGIN
        SELECT 0 AS Resultado, 'Error: No se puede eliminar. El ingrediente está siendo usado en una o más recetas.' AS Mensaje;
        RETURN;
    END;
 
    BEGIN TRY
        DELETE FROM Ingredientes WHERE IdIngrediente = @IdIngrediente;
        SELECT 1 AS Resultado, 'Ingrediente eliminado correctamente.' AS Mensaje;
    END TRY
    BEGIN CATCH
        SELECT 0 AS Resultado, 'Error al eliminar el ingrediente: ' + ERROR_MESSAGE() AS Mensaje;
    END CATCH
END;
GO

---6. SPs de Gestión de Recetas

-- 6.1. Insertar receta completa (cabecera + detalle)
CREATE PROCEDURE SP_InsertarRecetaCompleta
    @IdProducto INT,
    @DetalleRecetasXML XML
AS
BEGIN
    SET NOCOUNT ON;
    IF @IdProducto IS NULL OR @IdProducto <= 0 RETURN;
    IF NOT EXISTS (SELECT 1 FROM Productos WHERE IdProducto = @IdProducto AND Activo = 1) RETURN;
    IF EXISTS (SELECT 1 FROM Recetas WHERE IdProducto = @IdProducto) RETURN;
    IF @DetalleRecetasXML IS NULL OR @DetalleRecetasXML.exist('/Detalles/Detalle') = 0 RETURN;
 
    BEGIN TRANSACTION;
    BEGIN TRY
        INSERT INTO Recetas (IdProducto, FechaCreacion)
        VALUES (@IdProducto, GETDATE());
 
        DECLARE @NuevoIdReceta INT = SCOPE_IDENTITY();
 
        INSERT INTO DetalleRecetas (IdReceta, IdIngrediente, CantidadRequerida)
        SELECT @NuevoIdReceta, Tbl.Col.value('(IdIngrediente)[1]', 'INT'), Tbl.Col.value('(CantidadRequerida)[1]','DECIMAL(10,4)')
        FROM @DetalleRecetasXML.nodes('/Detalles/Detalle') AS Tbl(Col);
 
        COMMIT TRANSACTION;
        SELECT 1 AS Resultado, 'Receta registrada completamente con éxito.' AS Mensaje, @NuevoIdReceta AS IdReceta;
    END TRY
    BEGIN CATCH
        IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
        SELECT 0 AS Resultado, 'Fallo al guardar la receta: ' + ERROR_MESSAGE() AS Mensaje, NULL AS IdReceta;
    END CATCH
END;
GO
 
-- 6.2. Obtener detalle de receta por producto
CREATE PROCEDURE SP_ObtenerDetalleRecetaPorProducto
    @IdProducto INT
AS
BEGIN
    SET NOCOUNT ON;
 
    SELECT
        dr.IdDetalleReceta, r.IdReceta, dr.IdIngrediente, i.Nombre AS NombreIngrediente, dr.CantidadRequerida, i.UnidadMedida
    FROM Recetas r
    JOIN DetalleRecetas dr ON r.IdReceta = dr.IdReceta
    JOIN Ingredientes i ON dr.IdIngrediente = i.IdIngrediente
    WHERE r.IdProducto = @IdProducto;
END;
GO
 
-- 6.3. Eliminar receta completa
CREATE PROCEDURE SP_EliminarReceta
    @IdProducto INT
AS
BEGIN
    SET NOCOUNT ON;
    IF @IdProducto IS NULL OR @IdProducto <= 0 RETURN;
 
    DECLARE @IdReceta INT;
    SELECT @IdReceta = IdReceta FROM Recetas WHERE IdProducto = @IdProducto;
 
    IF @IdReceta IS NULL RETURN;
 
    BEGIN TRANSACTION;
    BEGIN TRY
        DELETE FROM DetalleRecetas WHERE IdReceta = @IdReceta;
        DELETE FROM Recetas WHERE IdReceta = @IdReceta;
        COMMIT TRANSACTION;
        SELECT 1 AS Resultado, 'Receta eliminada correctamente.' AS Mensaje;
    END TRY
    BEGIN CATCH
        IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
        SELECT 0 AS Resultado, 'Fallo al eliminar la receta: ' + ERROR_MESSAGE() AS Mensaje;
    END CATCH
END;
GO